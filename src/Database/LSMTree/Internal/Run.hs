{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Functionality related to LSM-Tree runs (sequences of LSM-Tree data).
--
-- === TODO
--
-- This is temporary module header documentation. The module will be
-- fleshed out more as we implement bits of it.
--
-- Related work packages: 5, 6
--
-- This module includes in-memory parts and I\/O parts for, amongst others,
--
-- * High-performance batch lookups
--
-- * Range lookups
--
-- * Incremental run construction
--
-- * Lookups in loaded disk pages, value resolution
--
-- * In-memory representation of a run
--
-- * Flushing a write buffer to a run
--
-- * Opening, deserialising, and verifying files for an LSM run.
--
-- * Closing runs (and removing files)
--
-- * high performance, incremental k-way merge
--
-- The above list is a sketch. Functionality may move around, and the list is
-- not exhaustive.
--
module Database.LSMTree.Internal.Run (
    -- * Run
    Run (..)
  , RunFsPaths
  , sizeInPages
  , addReference
  , removeReference
  , readBlob
    -- ** Run creation
  , fromMutable
  , fromWriteBuffer
  , openFromDisk
    -- ** RefCount
  , RefCount (..)
  , incRefCount
  , decRefCount
  ) where

import           Control.DeepSeq (NFData (rnf))
import           Control.Exception (Exception, finally, throwIO)
import           Control.Monad (when)
import           Data.BloomFilter (Bloom)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Foldable (for_)
import           Data.IORef
import           Data.Primitive.ByteArray (newPinnedByteArray,
                     unsafeFreezeByteArray)
import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import           Database.LSMTree.Internal.BloomFilter (bloomFilterFromSBS)
import           Database.LSMTree.Internal.ByteString (tryCheapToShort)
import qualified Database.LSMTree.Internal.CRC32C as CRC
import           Database.LSMTree.Internal.Entry (NumEntries (..))
import           Database.LSMTree.Internal.IndexCompact (IndexCompact, NumPages)
import qualified Database.LSMTree.Internal.IndexCompact as Index
import           Database.LSMTree.Internal.Paths
import qualified Database.LSMTree.Internal.RawBytes as RB
import           Database.LSMTree.Internal.RunBuilder (RunBuilder)
import qualified Database.LSMTree.Internal.RunBuilder as Builder
import           Database.LSMTree.Internal.Serialise
import           Database.LSMTree.Internal.WriteBuffer (WriteBuffer)
import qualified Database.LSMTree.Internal.WriteBuffer as WB
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import qualified System.FS.API as FS
import           System.FS.API (HasFS)
import qualified System.FS.BlockIO.API as FS
import           System.FS.BlockIO.API (HasBlockIO)


-- | The in-memory representation of a completed LSM run.
--
data Run fhandle = Run {
      runNumEntries :: !NumEntries
      -- | The reference count for the LSM run. This counts the
      -- number of references from LSM handles to this run. When
      -- this drops to zero the open files will be closed.
    , runRefCount   :: !(IORef RefCount)
      -- | The file system paths for all the files used by the run.
    , runRunFsPaths :: !RunFsPaths
      -- | The bloom filter for the set of keys in this run.
    , runFilter     :: !(Bloom SerialisedKey)
      -- | The in-memory index mapping keys to page numbers in the
      -- Key\/Ops file. In future we may support alternative index
      -- representations.
    , runIndex      :: !IndexCompact
      -- | The file handle for the Key\/Ops file. This file is opened
      -- read-only and is accessed in a page-oriented way, i.e. only
      -- reading whole pages, at page offsets. It will be opened with
      -- 'O_DIRECT' on supported platforms.
    , runKOpsFile   :: !fhandle
      -- | The file handle for the BLOBs file. This file is opened
      -- read-only and is accessed in a normal style using buffered
      -- I\/O, reading arbitrary file offset and length spans.
    , runBlobFile   :: !fhandle
    }

-- TODO: provide a proper instance that checks NoThunks for each field.
deriving via OnlyCheckWhnfNamed "Run" (Run h) instance NoThunks (Run h)

instance NFData fhandle => NFData (Run fhandle) where
  rnf (Run a b c d e f g) =
      rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq`
      rnf f `seq` rnf g

sizeInPages :: Run fhandle -> NumPages
sizeInPages = Index.sizeInPages . runIndex

-- | Increase the reference count by one.
addReference :: HasFS IO h -> Run (FS.Handle h) -> IO ()
addReference _ Run {..} =
    atomicModifyIORef' runRefCount (\c -> (incRefCount c, ()))

-- | Decrease the reference count by one.
-- After calling this operation, the run must not be used anymore.
-- If the reference count reaches zero, the run is closed, removing all its
-- associated files from disk.
removeReference :: HasFS IO h -> Run (FS.Handle h) -> IO ()
removeReference fs run@Run {..} = do
    count <- atomicModifyIORef' runRefCount ((\c -> (c, c)) . decRefCount)
    when (count <= RefCount 0) $
      close fs run

-- | The 'BlobSpan' to read must come from this run!
readBlob :: HasFS IO h -> Run (FS.Handle h) -> BlobSpan -> IO SerialisedBlob
readBlob fs Run {..} BlobSpan {..} = do
    let off = fromIntegral blobSpanOffset
    let len = fromIntegral blobSpanSize
    mba <- newPinnedByteArray len
    _ <- FS.hGetBufExactlyAt fs runBlobFile mba 0 (fromIntegral len) off
    ba <- unsafeFreezeByteArray mba
    let !rb = RB.fromByteArray 0 len ba
    return (SerialisedBlob rb)

-- | Close the files used in the run, but do not remove them from disk.
-- After calling this operation, the run must not be used anymore.
--
-- TODO: Once snapshots are implemented, files should get removed, but for now
-- we want to be able to re-open closed runs from disk.
close :: HasFS IO h -> Run (FS.Handle h) -> IO ()
close fs Run {..} = do
    FS.hClose fs runKOpsFile
      `finally`
        FS.hClose fs runBlobFile

-- | Create a run by finalising a mutable run.
fromMutable :: HasFS IO h -> HasBlockIO IO h -> RefCount -> RunBuilder (FS.Handle h) -> IO (Run (FS.Handle h))
fromMutable fs hbio refCount builder = do
    (runRunFsPaths, runFilter, runIndex, runNumEntries) <-
      Builder.unsafeFinalise fs builder
    runRefCount <- newIORef refCount
    runKOpsFile <- FS.hOpen fs (runKOpsPath runRunFsPaths) FS.ReadMode
    FS.writeNoCache hbio runKOpsFile True
    runBlobFile <- FS.hOpen fs (runBlobPath runRunFsPaths) FS.ReadMode
    return Run {..}


-- | Write a write buffer to disk, including the blobs it contains.
-- The resulting run has a reference count of 1.
--
-- TODO: As a possible optimisation, blobs could be written to a blob file
-- immediately when they are added to the write buffer, avoiding the need to do
-- it here.
fromWriteBuffer ::
     HasFS IO h
  -> HasBlockIO IO h
  -> RunFsPaths
  -> WriteBuffer
  -> IO (Run (FS.Handle h))
fromWriteBuffer fs hbio fsPaths buffer = do
    -- We just estimate the number of pages to be one, as the write buffer is
    -- expected to be small enough not to benefit from more precise tuning.
    -- More concretely, no range finder bits will be used anyways unless there
    -- are at least 2^16 pages.
    builder <- Builder.new fs fsPaths (WB.numEntries buffer) 1
    for_ (WB.toList buffer) $ \(k, e) ->
      Builder.addKeyOp fs builder k e
    fromMutable fs hbio (RefCount 1) builder

data ChecksumError = ChecksumError FS.FsPath CRC.CRC32C CRC.CRC32C
  deriving Show

instance Exception ChecksumError

data FileFormatError = FileFormatError FS.FsPath String
  deriving Show

instance Exception FileFormatError

-- | Load a previously written run from disk, checking each file's checksum
-- against the checksum file.
-- The resulting 'Run' has a reference count of 1.
--
-- Exceptions will be raised when any of the file's contents don't match their
-- checksum ('ChecksumError') or can't be parsed ('FileFormatError').
openFromDisk :: HasFS IO h -> RunFsPaths -> IO (Run (FS.Handle h))
openFromDisk fs runRunFsPaths = do
    expectedChecksums <-
       expectValidFile (runChecksumsPath runRunFsPaths) . fromChecksumsFile
         =<< CRC.readChecksumsFile fs (runChecksumsPath runRunFsPaths)

    -- verify checksums of files we don't read yet
    let paths = pathsForRunFiles runRunFsPaths
    checkCRC (forRunKOps expectedChecksums) (forRunKOps paths)
    checkCRC (forRunBlob expectedChecksums) (forRunBlob paths)

    -- read and try parsing files
    runFilter <-
      expectValidFile (forRunFilter paths) . bloomFilterFromSBS
        =<< readCRC (forRunFilter expectedChecksums) (forRunFilter paths)
    (runNumEntries, runIndex) <-
      expectValidFile (forRunIndex paths) . Index.fromSBS
        =<< readCRC (forRunIndex expectedChecksums) (forRunIndex paths)

    runKOpsFile <- FS.hOpen fs (runKOpsPath runRunFsPaths) FS.ReadMode
    runBlobFile <- FS.hOpen fs (runBlobPath runRunFsPaths) FS.ReadMode
    runRefCount <- newIORef (RefCount 1)
    return Run {..}
  where
    checkCRC :: CRC.CRC32C -> FS.FsPath -> IO ()
    checkCRC expected fp = do
        checksum <- CRC.readFileCRC32C fs fp
        expectChecksum fp expected checksum

    readCRC :: CRC.CRC32C -> FS.FsPath -> IO SBS.ShortByteString
    readCRC expected fp = FS.withFile fs fp FS.ReadMode $ \h -> do
        -- Read the whole file, which should usually return a single chunk.
        -- In this case, 'toStrict' is O(1).
        size <- FS.hGetSize fs h
        (lbs, crc) <- CRC.hGetExactlyCRC32C fs h size CRC.initialCRC32C
        expectChecksum fp expected crc
        let bs = LBS.toStrict lbs
        return $ case tryCheapToShort bs of
          Right sbs -> sbs
          Left _err -> SBS.toShort bs  -- Should not happen, but just in case

    expectChecksum fp expected checksum =
        when (expected /= checksum) $
          throwIO $ ChecksumError fp expected checksum

    expectValidFile _  (Right x)  = pure x
    expectValidFile fp (Left err) = throwIO $ FileFormatError fp err


-- | The 'Run' objects are reference counted.
newtype RefCount = RefCount Int
  deriving (Eq, Ord, Show)

incRefCount, decRefCount :: RefCount -> RefCount
incRefCount (RefCount n) = RefCount (n+1)
decRefCount (RefCount n) = RefCount (n-1)
