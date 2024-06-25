{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Database.LSMTree.Internal.Merge (
    Merge (..)
  , Level (..)
  , Mappend
  , new
  , close
  , StepResult (..)
  , steps
  ) where

import           Control.Exception (assert)
import           Control.Monad (when)
import           Data.Coerce (coerce)
import           Data.Traversable (for)
import           Database.LSMTree.Internal.BlobRef (BlobRef (..))
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.Run (Run)
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.RunBuilder (RunBuilder)
import qualified Database.LSMTree.Internal.RunBuilder as Builder
import qualified Database.LSMTree.Internal.RunReader as Reader
import qualified Database.LSMTree.Internal.RunReaders as Readers
import           Database.LSMTree.Internal.Serialise
import qualified System.FS.API as FS
import           System.FS.API (HasFS)
import           System.FS.BlockIO.API (HasBlockIO)

-- | An in-progress incremental k-way merge of 'Run's.
--
-- Since we always resolve all entries of the same key in one go, there is no
-- need to store incompletely-resolved entries.
--
-- TODO: Reference counting will have to be done somewhere, either here or in
-- the layer above.
data Merge fhandle = Merge {
      mergeLevel   :: !Level
    , mergeMappend :: !Mappend
    , mergeReaders :: {-# UNPACK #-} !(Readers.Readers fhandle)
    , mergeBuilder :: !(RunBuilder fhandle)
    }

data Level = MidLevel | LastLevel
  deriving (Eq, Show)

type Mappend = SerialisedValue -> SerialisedValue -> SerialisedValue

-- | Returns 'Nothing' if no input 'Run' contains any entries.
-- The list of runs should be sorted from new to old.
new ::
     HasFS IO h
  -> Level
  -> Mappend
  -> Run.RunFsPaths
  -> [Run (FS.Handle h)]
  -> IO (Maybe (Merge (FS.Handle h)))
new fs mergeLevel mergeMappend targetPaths runs = do
    mreaders <- Readers.new fs runs
    for mreaders $ \mergeReaders -> do
      -- calculate upper bounds based on input runs
      let numEntries = coerce (sum @[] @Int) (map Run.runNumEntries runs)
      let numPages = sum (map Run.sizeInPages runs)
      mergeBuilder <- Builder.new fs targetPaths numEntries numPages
      return Merge {..}

-- | This function should be called when discarding a 'Merge' before it
-- was done (i.e. returned 'MergeComplete'). This removes the incomplete files
-- created for the new run so far and avoids leaking file handles.
--
-- Once it has been called, do not use the 'Merge' any more!
close ::
     HasFS IO h
  -> Merge (FS.Handle h)
  -> IO ()
close fs Merge {..} = do
    Builder.close fs mergeBuilder
    Readers.close fs mergeReaders

data StepResult fhandle = MergeInProgress | MergeComplete !(Run fhandle)

stepsInvariant :: Int -> (Int, StepResult a) -> Bool
stepsInvariant requestedSteps = \case
    (n, MergeInProgress) -> n >= requestedSteps
    _                    -> True

-- | Do at least a given number of steps of merging. Each step reads a single
-- entry, then either resolves the previous entry with the new one or writes it
-- out to the run being created. Since we always finish resolving a key we
-- started, we might do slightly more work than requested.
--
-- Returns the number of input entries read, which is guaranteed to be at least
-- as many as requested (unless the merge is complete).
--
-- If this returns 'MergeComplete', do not use the `Merge` any more!
--
-- The resulting run has a reference count of 1.
steps ::
     HasFS IO h
  -> HasBlockIO IO h
  -> Merge (FS.Handle h)
  -> Int  -- ^ How many input entries to consume (at least)
  -> IO (Int, StepResult (FS.Handle h))
steps fs hbio Merge {..} requestedSteps =
    (\res -> assert (stepsInvariant requestedSteps res) res) <$> go 0
  where
    go !n
      | n >= requestedSteps =
          return (n, MergeInProgress)
      | otherwise = do
          (key, entry, hasMore) <- Readers.pop fs mergeReaders
          case hasMore of
            Readers.HasMore ->
              handleEntry (n + 1) key entry
            Readers.Drained -> do
              -- no future entries, no previous entry to resolve, just write!
              writeReaderEntry fs mergeLevel mergeBuilder key entry
              completeMerge (n + 1)

    handleEntry !n !key (Reader.Entry (Mupdate v)) =
        -- resolve small mupsert vals with the following entries of the same key
        handleMupdate n key v
    handleEntry !n !key (Reader.EntryOverflow (Mupdate v) _ len overflowPages) =
        -- resolve large mupsert vals with following entries of the same key
        handleMupdate n key (Reader.appendOverflow len overflowPages v)
    handleEntry !n !key entry = do
        -- otherwise, we can just drop all following entries of same key
        writeReaderEntry fs mergeLevel mergeBuilder key entry
        dropRemaining n key

    -- the value is from a mupsert, complete (not just a prefix)
    handleMupdate !n !key !v = do
        nextKey <- Readers.peekKey mergeReaders
        if nextKey /= key
          then do
            -- resolved all entries for this key, write it
            writeSerialisedEntry fs mergeLevel mergeBuilder key (Mupdate v)
            go n
          else do
            (_, nextEntry, hasMore) <- Readers.pop fs mergeReaders
            -- for resolution, we need the full second value to be present
            let resolved = combine mergeMappend
                             (Mupdate v)
                             (Reader.toFullEntry nextEntry)
            case hasMore of
              Readers.HasMore -> case resolved of
                Mupdate v' ->
                  -- still a mupsert, keep resolving
                  handleMupdate (n + 1) key v'
                _ -> do
                  -- done with this key, now the remaining entries are obsolete
                  writeSerialisedEntry fs mergeLevel mergeBuilder key resolved
                  dropRemaining (n + 1) key
              Readers.Drained -> do
                writeSerialisedEntry fs mergeLevel mergeBuilder key resolved
                completeMerge (n + 1)

    dropRemaining !n !key = do
        (dropped, hasMore) <- Readers.dropWhileKey fs mergeReaders key
        case hasMore of
          Readers.HasMore -> go (n + dropped)
          Readers.Drained -> completeMerge (n + dropped)

    completeMerge !n = do
        -- All Readers have been drained, the builder finalised.
        -- No further cleanup required.
        run <- Run.fromMutable fs hbio (Run.RefCount 1) mergeBuilder
        return (n, MergeComplete run)


writeReaderEntry ::
     HasFS IO h
  -> Level
  -> RunBuilder (FS.Handle h)
  -> SerialisedKey
  -> Reader.Entry (FS.Handle h)
  -> IO ()
writeReaderEntry fs level builder key (Reader.Entry entryFull) =
      -- Small entry.
      -- Note that this small entry could be the only one on the page. We only
      -- care about it being small, not single-entry, since it could still end
      -- up sharing a page with other entries in the merged run.
      -- TODO(optimise): This doesn't fully exploit the case where there is a
      -- single page small entry on the page which again ends up as the only
      -- entry of a page (which would for example happen a lot if most entries
      -- have 2k-4k bytes). In that case we could have copied the RawPage
      -- (but we find out too late to easily exploit it).
      writeSerialisedEntry fs level builder key entryFull
writeReaderEntry fs level builder key entry@(Reader.EntryOverflow prefix page _ overflowPages)
  | InsertWithBlob {} <- prefix =
      assert (shouldWriteEntry level prefix) $  -- large, can't be delete
        -- has blob, we can't just copy the first page, fall back
        -- we simply append the overflow pages to the value
        Builder.addKeyOp fs builder key
          =<< traverse resolveBlobRef (Reader.toFullEntry entry)
        -- TODO(optimise): This copies the overflow pages unnecessarily.
        -- We could extend the RunBuilder API to allow to either:
        -- 1. write an Entry (containing the value prefix) + [RawOverflowPage]
        -- 2. write a RawPage + SerialisedBlob + [RawOverflowPage], rewriting
        --      the raw page's blob offset (slightly faster, but a bit hacky)
  | otherwise =
      assert (shouldWriteEntry level prefix) $  -- large, can't be delete
        -- no blob, directly copy all pages as they are
        Builder.addLargeSerialisedKeyOp fs builder key page overflowPages
  where
    resolveBlobRef (BlobRef run blobSpan) =
      Run.readBlob fs run blobSpan

writeSerialisedEntry ::
     HasFS IO h
  -> Level
  -> RunBuilder (FS.Handle h)
  -> SerialisedKey
  -> Entry SerialisedValue (BlobRef (Run (FS.Handle h)))
  -> IO ()
writeSerialisedEntry fs level builder key entry =
    when (shouldWriteEntry level entry) $
      Builder.addKeyOp fs builder key =<< traverse resolveBlobRef entry
  where
    resolveBlobRef (BlobRef run blobSpan) =
      Run.readBlob fs run blobSpan

-- One the last level we could also turn Mupdate into Insert,
-- but no need to complicate things.
shouldWriteEntry :: Level -> Entry v b -> Bool
shouldWriteEntry level = \case
    Delete -> level == MidLevel
    _      -> True
