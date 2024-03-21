{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- | A run that is being read incrementally
--
module Database.LSMTree.Internal.Run.Reader (
    Reader
  , new
  , readNext
  , ReaderResult (..)
  , resolveBlobSpan
  -- , close
  ) where

import           Control.Monad (when)
import           Control.Monad.Primitive (RealWorld)
import qualified Control.Monad.ST as ST
import qualified Data.ByteString.Builder as BSB
import           Data.Foldable (for_, traverse_)
import           Data.IORef
import           Data.Maybe (fromJust)  -- TODO
import           Data.Primitive.PrimVar
import           Data.Traversable (for)
import           Data.Word (Word16, Word64)
import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import           Database.LSMTree.Internal.BloomFilter (bloomFilterToBuilder)
import           Database.LSMTree.Internal.CRC32C (CRC32C)
import qualified Database.LSMTree.Internal.CRC32C as CRC
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.RawPage
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.Run.BloomFilter (Bloom)
import           Database.LSMTree.Internal.Run.Construction (RunAcc)
import qualified Database.LSMTree.Internal.Run.Construction as Cons
import           Database.LSMTree.Internal.Run.FsPaths
import           Database.LSMTree.Internal.Run.Index.Compact (CompactIndex,
                     NumPages)
import qualified Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise
import qualified System.FS.API as FS
import           System.FS.API (HasFS)


-- Reader ---------------------------------------------------------------------

-- | Keeps open a reference to the run, remember to close!
--
-- \"Current\" value refers to the next one to be read/yielded.
-- New pages are loaded when trying to read their first entry.
data Reader fhandle = Reader {
      readerCurrentPage :: !(IORef RawPage)
    -- TODO: handle offset through file handle or explicitly track?
    -- , readerCurrentPageNo :: !(PrimVar RealWorld Int)
    , readerCurrentEntryNo :: !(PrimVar RealWorld Word16)
    , readerKOpsHandle :: !fhandle
    , readerBlobHandle :: !fhandle
    }

new ::
     HasFS IO h
  -> Run.Run (FS.Handle h)
  -> IO (Reader (FS.Handle h))
new fs run = do
    -- increase ref counter
    -- load first page from disk
    readerKOpsHandle <- FS.hOpen fs (runKOpsPath (Run.lsmRunFsPaths run)) FS.ReadMode
    readerBlobHandle <- FS.hOpen fs (runBlobPath (Run.lsmRunFsPaths run)) FS.ReadMode
    readerCurrentEntryNo <- newPrimVar 0
    firstPage <- fromJust <$> readDiskPage fs readerKOpsHandle
    readerCurrentPage <- newIORef firstPage
    return Reader {..}

data ReaderResult key entry =
      ReaderClosed
    | ReadNormalEntry key entry
    | ReadOverflowEntry key entry [RawPage]  -- TODO: use OverflowPage type

-- TODO: handle overflow pages
-- TODO: resolve blobs here or not?
-- Don't call after getting 'ReaderClosed'!
readNext ::
     HasFS IO h
  -> Reader (FS.Handle h)
  -> IO (ReaderResult SerialisedKey (Entry SerialisedValue BlobSpan))
readNext fs reader@Reader {..} = do
    entryNo <- readPrimVar readerCurrentEntryNo
    page <- readIORef readerCurrentPage
    -- take entry from current page (resolve blob if necessary)
    case rawPageIndex page entryNo of
      IndexNotPresent -> do
        -- if it is past the last one, load a new page from disk
        readDiskPage fs readerKOpsHandle >>= \case
          Nothing -> do
            -- if there are no more pages, close
            -- TODO: close file handles
            return ReaderClosed
          Just newPage -> do
            writeIORef readerCurrentPage newPage
            writePrimVar readerCurrentEntryNo 0
            -- TODO(optimise): wastes a few ref/var reads etc.
            readNext fs reader
      IndexEntry key entry -> do
        return (ReadNormalEntry key entry)
      IndexEntryOverflow key entryPrefix suffix -> do
        -- TODO: larger-than-page value with blob?
        let overflowPages = undefined -- TODO
        return (ReadOverflowEntry key entryPrefix overflowPages)

-- TODO
resolveBlobSpan :: HasFS IO h -> Reader (FS.Handle h) -> BlobSpan -> IO SerialisedBlob
resolveBlobSpan = undefined

-- TODO
readDiskPage :: HasFS IO h -> FS.Handle h -> IO (Maybe RawPage)
readDiskPage = undefined
