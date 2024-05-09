{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Database.LSMTree.Internal.Merge (
    Merge (..)
  , Level (..)
  , Mappend
  , new
  , StepResult (..)
  , steps
  , close

    -- TODO: internal?
  , HasMore (..)
  , Readers
  , readersNew
  , readersPeekKey
  , readersPop
  , readersDropWhileKey
  , readersClose
  ) where

import           Control.Exception (assert)
import           Control.Monad (when, zipWithM)
import           Control.Monad.Primitive (RealWorld)
import           Data.Coerce (coerce)
import           Data.Function (on)
import           Data.IORef
import           Data.Maybe (catMaybes)
import           Data.Traversable (for)
import           Database.LSMTree.Internal.BlobRef (BlobRef (..))
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.Run (Run)
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.RunBuilder (RunBuilder)
import qualified Database.LSMTree.Internal.RunBuilder as Builder
import           Database.LSMTree.Internal.RunReader (RunReader)
import qualified Database.LSMTree.Internal.RunReader as Reader
import           Database.LSMTree.Internal.Serialise
import qualified KMerge.Heap as Heap
import qualified System.FS.API as FS
import           System.FS.API (HasFS)

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
    , mergeReaders :: {-# UNPACK #-} !(Readers fhandle)
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
    mreaders <- readersNew fs runs
    for mreaders $ \mergeReaders -> do
      -- calculate upper bounds based on input runs
      let numEntries = coerce (sum @[] @Int) (map Run.runNumEntries runs)
      let numPages = sum (map Run.sizeInPages runs)
      mergeBuilder <- Builder.new fs targetPaths numEntries numPages

      return Merge {..}

data StepResult fhandle = MergeInProgress | MergeComplete !(Run fhandle)

stepsInvariant :: Int -> (Int, StepResult a) -> Bool
stepsInvariant requestedSteps = \case
    (n, MergeInProgress) -> n >= requestedSteps
    _                    -> True

-- | Do at least a given number of steps of merging. Each step reads a single
-- entry, then either resolves the previous entry with the new one or writes it
-- out to the run being created.
-- Returns the number of input entries read, which is guaranteed to be at least
-- as many as requested (unless the merge is complete).
--
-- If this returns 'MergeComplete', do not use the `Merge` any more!
--
-- The resulting run has a reference count of 1.
steps ::
     HasFS IO h
  -> Merge (FS.Handle h)
  -> Int  -- ^ How many input entries to consume (at least)
  -> IO (Int, StepResult (FS.Handle h))
steps fs Merge {..} requestedSteps =
    (\res -> assert (stepsInvariant requestedSteps res) res) <$> go 0
  where
    go !n
      | n >= requestedSteps =
          return (n, MergeInProgress)
      | otherwise = do
          (key, entry, hasMore) <- readersPop fs mergeReaders
          case hasMore of
            HasMore ->
              handleEntry (n + 1) key entry
            Drained -> do
              -- no future entries, no previous entry to resolve, just write!
              writeRawEntry fs mergeLevel mergeBuilder key entry
              completeMerge (n + 1)

    handleEntry !n !key (Reader.RawEntry (Mupdate v)) =
        -- resolve small mupsert vals with the following entries of the same key
        handleMupdate n key v
    handleEntry !n !key (Reader.RawEntryOverflow (Mupdate v) _ len overflowPages) =
        -- resolve large mupsert vals with following entries of the same key
        handleMupdate n key (Reader.appendOverflow len overflowPages v)
    handleEntry !n !key entry = do
        -- otherwise, we can just drop all following entries of same key
        writeRawEntry fs mergeLevel mergeBuilder key entry
        dropRemaining n key

    -- the value is from a mupsert, complete (not just a prefix)
    handleMupdate !n !key !v = do
        nextKey <- readersPeekKey mergeReaders
        if nextKey /= key
          then do
            -- resolved all entries for this key, write it
            writeSerialisedEntry fs mergeLevel mergeBuilder key (Mupdate v)
            go n
          else do
            (_, nextEntry, hasMore) <- readersPop fs mergeReaders
            -- for resolution, we need the full second value to be present
            let resolved = combine mergeMappend
                             (Mupdate v)
                             (Reader.rawToFullEntry nextEntry)
            case hasMore of
              HasMore -> case resolved of
                Mupdate v' ->
                  -- still a mupsert, keep resolving
                  handleMupdate (n + 1) key v'
                _ -> do
                  -- done with this key, now the remaining entries are obsolete
                  writeSerialisedEntry fs mergeLevel mergeBuilder key resolved
                  dropRemaining (n + 1) key
              Drained -> do
                writeSerialisedEntry fs mergeLevel mergeBuilder key resolved
                completeMerge (n + 1)

    dropRemaining !n !key = do
        (dropped, hasMore) <- readersDropWhileKey fs mergeReaders key
        case hasMore of
          HasMore -> go (n + dropped)
          Drained -> completeMerge (n + dropped)

    completeMerge !n = do
        -- All Readers have been drained, the builder finalised.
        -- No further cleanup required.
        run <- Run.fromMutable fs (Run.RefCount 1) mergeBuilder
        return (n, MergeComplete run)

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
    readersClose fs mergeReaders

{-------------------------------------------------------------------------------
  Read
-------------------------------------------------------------------------------}

-- | Abstraction for the collection of 'RunReader', yielding elements in order.
-- More precisely, that means first ordered by their key, then by the input
-- run they came from. This is important for resolving multiple entries with the
-- same key into one.
--
-- Construct with 'readersNew', then keep calling 'readersPop'.
-- If aborting early, remember to call 'readersClose'!
data Readers fhandle = Readers {
      readersHeap :: !(Heap.MutableHeap RealWorld (ReadCtx fhandle))
      -- | Since there is always one reader outside of the heap, we need to
      -- store it separately. This also contains the next k\/op to yield, unless
      -- all readers are drained, i.e. both:
      -- 1. the reader it contains is empty
      -- 2. the heap is empty
    , readersNext :: !(IORef (ReadCtx fhandle))
    }

newtype ReaderNumber = ReaderNumber Int
  deriving (Eq, Ord)

-- | Each heap element needs some more context than just the reader.
-- E.g. the 'Eq' instance we need to be able to access the first key to be read
-- in a pure way.
--
-- TODO(optimisation): We allocate this record for each k/op. This might be
-- avoidable, see ideas below.
data ReadCtx fhandle = ReadCtx {
      -- We could avoid this using a more specialised mutable heap with separate
      -- arrays for keys and values (or even each of their components).
      -- Using an 'STRef' could avoid reallocating the record for every entry,
      -- but that might not be straightforward to integrate with the heap.
      readCtxHeadKey   :: !SerialisedKey
    , readCtxHeadEntry :: !(Reader.RawEntry fhandle)
      -- We could get rid of this by making 'LoserTree' stable (for which there
      -- is a prototype already).
      -- Alternatively, if we decide to have an invariant that the number in
      -- 'RunFsPaths' is always higher for newer runs, then we could use that
      -- in the 'Ord' instance.
    , readCtxNumber    :: !ReaderNumber
    , readCtxReader    :: !(RunReader fhandle)
    }

instance Eq (ReadCtx fhandle) where
  (==) = (==) `on` (\r -> (readCtxHeadKey r, readCtxNumber r))

-- | Makes sure we resolve entries in the right order.
instance Ord (ReadCtx fhandle) where
  compare = compare `on` (\r -> (readCtxHeadKey r, readCtxNumber r))

-- | On equal keys, elements from runs earlier in the list are yielded first.
-- This means that the list of runs should be sorted from new to old.
readersNew ::
     HasFS IO h
  -> [Run (FS.Handle h)]
  -> IO (Maybe (Readers (FS.Handle h)))
readersNew fs runs = do
    readers <- zipWithM (fromRun . ReaderNumber) [1..] runs
    (readersHeap, firstReadCtx) <- Heap.newMutableHeap (catMaybes readers)
    for firstReadCtx $ \readCtx -> do
      readersNext <- newIORef readCtx
      return Readers {..}
  where
    fromRun n run = nextReadCtx fs n =<< Reader.new fs run

readersPeekKey ::
     Readers (FS.Handle h)
  -> IO SerialisedKey
readersPeekKey Readers {..} = do
    readCtxHeadKey <$> readIORef readersNext

-- | Once a function returned 'Drained', do not use the 'Readers' any more!
data HasMore = HasMore | Drained
  deriving (Eq, Show)

readersPop ::
     HasFS IO h
  -> Readers (FS.Handle h)
  -> IO (SerialisedKey, Reader.RawEntry (FS.Handle h), HasMore)
readersPop fs r@Readers {..} = do
    ReadCtx {..} <- readIORef readersNext
    hasMore <- readersDrop fs r readCtxNumber readCtxReader
    return (readCtxHeadKey, readCtxHeadEntry, hasMore)

readersDrop ::
     HasFS IO h
  -> Readers (FS.Handle h)
  -> ReaderNumber
  -> RunReader (FS.Handle h)
  -> IO HasMore
readersDrop fs Readers {..} number reader = do
    mNext <- nextReadCtx fs number reader >>= \case
      Nothing  -> Heap.extract readersHeap
      Just ctx -> Just <$> Heap.replaceRoot readersHeap ctx
    case mNext of
      Nothing ->
        return Drained
      Just next -> do
        writeIORef readersNext next
        return HasMore

readersDropWhileKey ::
     HasFS IO h
  -> Readers (FS.Handle h)
  -> SerialisedKey
  -> IO (Int, HasMore)  -- ^ How many were dropped?
readersDropWhileKey fs Readers {..} key = do
    cur <- readIORef readersNext
    if readCtxHeadKey cur == key
      then go 0 cur
      else return (0, HasMore)  -- nothing to do
  where
    -- invariant: @readCtxHeadKey == key@
    go !n ReadCtx {readCtxNumber, readCtxReader} = do
        mNext <- nextReadCtx fs readCtxNumber readCtxReader >>= \case
          Nothing  -> Heap.extract readersHeap
          Just ctx -> Just <$> Heap.replaceRoot readersHeap ctx
        let !n' = n + 1
        case mNext of
          Nothing -> do
            return (n', Drained)
          Just next -> do
            -- hasMore
            if readCtxHeadKey next == key
              then
                go n' next
              else do
                writeIORef readersNext next
                return (n', HasMore)

nextReadCtx ::
     HasFS IO h
  -> ReaderNumber
  -> RunReader (FS.Handle h)
  -> IO (Maybe (ReadCtx (FS.Handle h)))
nextReadCtx fs readCtxNumber readCtxReader = do
    res <- Reader.next fs readCtxReader
    case res of
      Reader.Empty -> do
        return Nothing
      Reader.ReadEntry readCtxHeadKey readCtxHeadEntry ->
        return (Just ReadCtx {..})

-- | Only call when aborting before all readers have been drained.
readersClose ::
     HasFS IO h
  -> Readers (FS.Handle h)
  -> IO ()
readersClose fs Readers {..} = do
    ReadCtx {readCtxReader} <- readIORef readersNext
    Reader.close fs readCtxReader
    closeHeap
  where
    closeHeap =
        Heap.extract readersHeap >>= \case
          Nothing -> return ()
          Just ReadCtx {readCtxReader} -> do
            Reader.close fs readCtxReader
            closeHeap

{-------------------------------------------------------------------------------
  Write
-------------------------------------------------------------------------------}

writeRawEntry ::
     HasFS IO h
  -> Level
  -> RunBuilder (FS.Handle h)
  -> SerialisedKey
  -> Reader.RawEntry (FS.Handle h)
  -> IO ()
writeRawEntry fs level builder key (Reader.RawEntry entryFull) =
      -- small
      -- Note that this small entry could be the only one on the page. We only
      -- care about it being small, not single-entry, since it could still end
      -- up sharing a page with other entries in the merged run.
      -- TODO(optimise): This doesn't fully exploit the case where there is a
      -- single page small entry on the page which again ends up as the only
      -- entry of a page. In that case we could have copied the RawPage (but we
      -- find out too late to easily exploit it).
      writeSerialisedEntry fs level builder key entryFull
writeRawEntry fs level builder key entry@(Reader.RawEntryOverflow prefix page _ overflowPages)
  | InsertWithBlob {} <- prefix =
      assert (shouldWriteEntry level prefix) $  -- large, can't be delete
        -- has blob, we can't just copy the first page, fall back
        -- we simply append the overflow pages to the value
        Builder.addKeyOp fs builder key
          =<< traverse resolveBlobRef (Reader.rawToFullEntry entry)
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
