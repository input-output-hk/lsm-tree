{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- | A run that is being read incrementally.
--
module Database.LSMTree.Internal.RunReader (
    RunReader
  , new
  , next
  , close
  , Result (..)
  , RawEntry (..)
  , rawEntryInvariant
  , rawToFullEntry
  , appendOverflow
  ) where

import           Control.Exception (assert, handleJust)
import           Control.Monad (guard)
import           Control.Monad.Primitive (RealWorld)
import           Data.Bifunctor (first)
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           Data.Primitive.ByteArray (newPinnedByteArray,
                     unsafeFreezeByteArray)
import           Data.Primitive.PrimVar
import           Data.Word (Word16, Word32)
import           Database.LSMTree.Internal.BitMath (ceilDivPageSize,
                     roundUpToPageSize)
import           Database.LSMTree.Internal.BlobRef (BlobRef (..))
import           Database.LSMTree.Internal.Entry
import qualified Database.LSMTree.Internal.RawBytes as RB
import           Database.LSMTree.Internal.RawOverflowPage (RawOverflowPage,
                     pinnedByteArrayToOverflowPages, rawOverflowPageRawBytes)
import           Database.LSMTree.Internal.RawPage
import           Database.LSMTree.Internal.Run (Run)
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.RunFsPaths
import           Database.LSMTree.Internal.Serialise
import qualified System.FS.API as FS
import           System.FS.API (HasFS)

-- | Allows reading the k\/ops of a run incrementally, using its own read-only
-- file handle and in-memory cache of the current disk page.
--
-- Creating a 'RunReader' does not increase the run's reference count, so make
-- sure the run remains open while using the reader.
--
-- New pages are loaded when trying to read their first entry.
--
-- TODO(optimise): Reuse page buffers using some kind of allocator. However,
-- deciding how long a page needs to stay around is not trivial.
data RunReader fhandle = RunReader {
      readerCurrentPage    :: !(IORef RawPage)
      -- | The index of the entry to be returned by the next call to 'next'.
    , readerCurrentEntryNo :: !(PrimVar RealWorld Word16)
      -- | Read mode file handle into the run's k\/ops file. We rely on it to
      -- track the position of the next disk page to read, instead of keeping
      -- a counter ourselves. Also, the run's handle is supposed to be opened
      -- with @O_DIRECT@, which is counterproductive here.
    , readerKOpsHandle     :: !fhandle
      -- | The run this reader is reading from.
    , readerRun            :: !(Run.Run fhandle)
    }

new ::
     HasFS IO h
  -> Run.Run (FS.Handle h)
  -> IO (RunReader (FS.Handle h))
new fs readerRun = do
    readerKOpsHandle <-
      FS.hOpen fs (runKOpsPath (Run.runRunFsPaths readerRun)) FS.ReadMode
    readerCurrentEntryNo <- newPrimVar 0
    -- load first page from disk, if it exists.
    firstPage <- fromMaybe emptyRawPage <$> readDiskPage fs readerKOpsHandle
    readerCurrentPage <- newIORef firstPage
    return RunReader {..}

-- | This function should be called when discarding a 'RunReader' before it
-- was done (i.e. returned 'Empty'). This avoids leaking file handles.
-- Once it has been called, do not use the reader any more!
close ::
     HasFS IO h
  -> RunReader (FS.Handle h)
  -> IO ()
close fs RunReader {..} = do
    FS.hClose fs readerKOpsHandle

-- | The 'SerialisedKey' and 'SerialisedValue' point into the in-memory disk
-- page. Keeping them alive will also prevent garbage collection of the 4k byte
-- array, so if they're long-lived, consider making a copy!
data Result fhandle
  = Empty
  | ReadEntry !SerialisedKey !(RawEntry fhandle)

data RawEntry fhandle =
    RawEntry
      !(Entry SerialisedValue (BlobRef (Run fhandle)))
  | -- | A large entry. The caller might be interested in various different
    -- (redundant) representation, so we return all of them.
    RawEntryOverflow
      -- | The value is just a prefix, with the remainder in the overflow pages.
      !(Entry SerialisedValue (BlobRef (Run fhandle)))
      -- | A page containing the single entry (or rather its prefix).
      !RawPage
      -- | Length of the overflow in bytes.
      !Word32
      -- | The overflow pages containing the suffix of the value.
      --
      -- TODO(optimise): Sometimes, reading the overflow pages is not necessary.
      -- We could just return the page index and offer a separate function to do
      -- the disk I/O once needed.
      ![RawOverflowPage]

rawEntryInvariant :: RawEntry fhandle -> Bool
rawEntryInvariant RawEntry {} = True
rawEntryInvariant (RawEntryOverflow _ page overflowBytes overflowPages) =
       overflowBytes > 0
    && rawPageOverflowPages page == ceilDivPageSize (fromIntegral overflowBytes)
    && rawPageOverflowPages page == length overflowPages

{-# INLINE rawToFullEntry #-}
rawToFullEntry :: RawEntry fhandle -> Entry SerialisedValue (BlobRef (Run fhandle))
rawToFullEntry entry =
    assert (rawEntryInvariant entry) $
      case entry of
        RawEntry e ->
          e
        RawEntryOverflow prefix _ len overflowPages ->
          first (appendOverflow len overflowPages) prefix

{-# INLINE appendOverflow #-}
appendOverflow :: Word32 -> [RawOverflowPage] -> SerialisedValue -> SerialisedValue
appendOverflow len overflowPages (SerialisedValue prefix) =
    SerialisedValue $
      RB.take (RB.size prefix + fromIntegral len) $
        mconcat (prefix : map rawOverflowPageRawBytes overflowPages)

-- | Stop using the 'RunReader' after getting 'Empty', because the 'Reader' is
-- automatically closed!
next ::
     HasFS IO h
  -> RunReader (FS.Handle h)
  -> IO (Result (FS.Handle h))
next fs reader@RunReader {..} = do
    entryNo <- readPrimVar readerCurrentEntryNo
    page <- readIORef readerCurrentPage
    go entryNo page
  where
    go !entryNo !page =
        -- take entry from current page (resolve blob if necessary)
        case rawPageIndex page entryNo of
          IndexNotPresent -> do
            -- if it is past the last one, load a new page from disk, try again
            readDiskPage fs readerKOpsHandle >>= \case
              Nothing -> do
                close fs reader
                return Empty
              Just newPage -> do
                writeIORef readerCurrentPage newPage
                writePrimVar readerCurrentEntryNo 0
                go 0 newPage  -- try again on the new page
          IndexEntry key entry -> do
            modifyPrimVar readerCurrentEntryNo (+1)
            let entry' = fmap (BlobRef readerRun) entry
            let rawEntry = RawEntry entry'
            assert (rawEntryInvariant rawEntry) $
              return (ReadEntry key rawEntry)
          IndexEntryOverflow key entry lenSuffix -> do
            -- TODO: we know that we need the next page, could already load?
            modifyPrimVar readerCurrentEntryNo (+1)
            let entry' = fmap (BlobRef readerRun) entry
            overflowPages <- readOverflowPages fs readerKOpsHandle lenSuffix
            let rawEntry = RawEntryOverflow entry' page lenSuffix overflowPages
            assert (rawEntryInvariant rawEntry) $
              return (ReadEntry key rawEntry)

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | Returns 'Nothing' on EOF.
readDiskPage :: HasFS IO h -> FS.Handle h -> IO (Maybe RawPage)
readDiskPage fs h = do
    mba <- newPinnedByteArray pageSize
    handleJust (guard . FS.isFsErrorType FS.FsReachedEOF) (\_ -> pure Nothing) $ do
      _ <- FS.hGetBufExactly fs h mba 0 (fromIntegral pageSize)
      ba <- unsafeFreezeByteArray mba
      let !rawPage = unsafeMakeRawPage ba 0
      return (Just rawPage)

pageSize :: Int
pageSize = 4096

-- | Throws exception on EOF. If a suffix was expected, the file should have it.
-- Reads full pages, despite the suffix only using part of the last page.
readOverflowPages :: HasFS IO h -> FS.Handle h -> Word32 -> IO [RawOverflowPage]
readOverflowPages fs h len = do
    let lenPages = fromIntegral (roundUpToPageSize len)  -- always read whole pages
    mba <- newPinnedByteArray lenPages
    _ <- FS.hGetBufExactly fs h mba 0 (fromIntegral lenPages)
    ba <- unsafeFreezeByteArray mba
    -- should not copy since 'ba' is pinned and its length is a multiple of 4k.
    return $ pinnedByteArrayToOverflowPages 0 lenPages ba
