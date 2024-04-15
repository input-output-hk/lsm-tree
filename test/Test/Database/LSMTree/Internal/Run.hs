{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Database.LSMTree.Internal.Run (
    -- * Main test tree
    tests,
) where

import           Data.Bifoldable (bifoldMap)
import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Coerce (coerce)
import           Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Primitive.ByteArray as BA
import           System.FilePath
import qualified System.FS.API as FS
import qualified System.FS.API.Lazy as FS
import qualified System.FS.IO as FsIO
import qualified System.FS.Sim.Error as FsSim
import qualified System.FS.Sim.MockFS as FsSim
import           System.IO.Temp
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase, (@=?), (@?))
import           Test.Tasty.QuickCheck

import           Database.LSMTree.Generators (KeyForIndexCompact (..),
                     LargeRawBytes (..))
import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import qualified Database.LSMTree.Internal.CRC32C as CRC
import           Database.LSMTree.Internal.Entry
import qualified Database.LSMTree.Internal.Normal as N
import qualified Database.LSMTree.Internal.RawBytes as RB
import           Database.LSMTree.Internal.RawPage
import           Database.LSMTree.Internal.Run
import           Database.LSMTree.Internal.Serialise
import           Database.LSMTree.Internal.WriteBuffer (WriteBuffer)
import qualified Database.LSMTree.Internal.WriteBuffer as WB
import           Database.LSMTree.Util (showPowersOf10)

import qualified FormatPage as Proto

import           Test.Database.LSMTree.Internal.IndexCompact ()

tests :: TestTree
tests = testGroup "Database.LSMTree.Internal.Run"
    [ testGroup "Write buffer to disk"
      [ testCase "Single insert (small)" $ do
          withSessionDir $ \sessionRoot ->
            testSingleInsert sessionRoot
              (mkKey "test-key")
              (mkVal "test-value")
              Nothing
      , testCase "Single insert (blob)" $ do
          withSessionDir $ \sessionRoot -> do
            testSingleInsert sessionRoot
              (mkKey "test-key")
              (mkVal "test-value")
              (Just (mkBlob "test-blob"))
      , testCase "Single insert (larger-than-page)" $ do
          withSessionDir $ \sessionRoot -> do
            testSingleInsert sessionRoot
              (mkKey "test-key")
              (mkVal ("test-value-" <> BS.concat (replicate 500 "0123456789")))
              Nothing
      , testProperty "Written pages can be read again" $ \wb ->
            prop_WriteAndRead wb
      , testProperty "A run can be written and loaded from disk" $ \wb ->
            prop_WriteAndLoad wb
      ]
    ]
  where
    withSessionDir = withTempDirectory "" "session"

    mkKey = SerialisedKey . RB.fromByteString
    mkVal = SerialisedValue . RB.fromByteString
    mkBlob = SerialisedBlob . RB.fromByteString

-- | Runs in IO, with a real file system.
testSingleInsert :: FilePath -> SerialisedKey -> SerialisedValue -> Maybe SerialisedBlob -> IO ()
testSingleInsert sessionRoot key val mblob = do
    let fs = FsIO.ioHasFS (FS.MountPoint sessionRoot)
    -- flush write buffer
    let wb = WB.addEntryNormal key (N.Insert val mblob) WB.empty
    _ <- fromWriteBuffer fs (RunFsPaths 42) wb
    -- check all files have been written
    let activeDir = sessionRoot </> "active"
    bsKOps <- BS.readFile (activeDir </> "42.keyops")
    bsBlobs <- BS.readFile (activeDir </> "42.blobs")
    bsFilter <- BS.readFile (activeDir </> "42.filter")
    bsIndex <- BS.readFile (activeDir </> "42.index")
    not (BS.null bsKOps) @? "k/ops file is empty"
    null mblob @=? BS.null bsBlobs  -- blob file might be empty
    not (BS.null bsFilter) @? "filter file is empty"
    not (BS.null bsIndex) @? "index file is empty"
    -- checksums
    checksums <- CRC.readChecksumsFile fs (FS.mkFsPath ["active", "42.checksums"])
    Map.lookup (CRC.ChecksumsFileName "keyops") checksums
      @=? Just (CRC.updateCRC32C bsKOps CRC.initialCRC32C)
    Map.lookup (CRC.ChecksumsFileName "blobs") checksums
      @=? Just (CRC.updateCRC32C bsBlobs CRC.initialCRC32C)
    Map.lookup (CRC.ChecksumsFileName "filter") checksums
      @=? Just (CRC.updateCRC32C bsFilter CRC.initialCRC32C)
    Map.lookup (CRC.ChecksumsFileName "index") checksums
      @=? Just (CRC.updateCRC32C bsIndex CRC.initialCRC32C)
    -- check page
    let page = rawPageFromByteString bsKOps 0
    1 @=? rawPageNumKeys page

    let pagesize :: Int
        Just pagesize =
           Proto.pageSizeBytes <$> Proto.calcPageSize
             (Proto.PageLogical
               [ ( Proto.Key (coerce RB.toByteString key)
                 , Proto.Insert (Proto.Value (coerce RB.toByteString val))
                 , Nothing ) ])
        suffix, prefix :: Int
        suffix = max 0 (pagesize - 4096)
        prefix = coerce RB.size val - suffix
    let expectedEntry = case mblob of
          Nothing -> Insert         (coerce RB.take prefix val)
          Just b  -> InsertWithBlob (coerce RB.take prefix val) b
    let expectedResult
          | suffix > 0 = LookupEntryOverflow expectedEntry (fromIntegral suffix)
          | otherwise  = LookupEntry         expectedEntry

    let actualEntry = fmap (readBlob bsBlobs) <$> rawPageLookup page key

    -- the lookup result is as expected, possibly with a prefix of the value
    expectedResult @=? actualEntry

    -- the value is as expected, including any overflow suffix
    let valPrefix = coerce RB.take prefix val
        valSuffix = (RB.fromByteString . BS.take suffix . BS.drop 4096) bsKOps
    val @=? SerialisedValue (valPrefix <> valSuffix)

    -- blob sanity checks
    length mblob @=? fromIntegral (rawPageNumBlobs page)

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Runs in IO, but using a mock file system.
--
-- TODO: Also test file system errors.
prop_WriteAndRead :: WriteBuffer KeyForIndexCompact LargeRawBytes SerialisedBlob -> Property
prop_WriteAndRead wb = ioProperty $ do
    fs <- FsSim.mkSimErrorHasFS' FsSim.empty FsSim.emptyErrors
    -- flush write buffer
    let fsPaths = RunFsPaths 42
    _ <- fromWriteBuffer fs fsPaths wb
    -- read pages
    bsBlobs <- getFile fs (runBlobPath fsPaths)
    bsKOps <- getFile fs (runKOpsPath fsPaths)
    let pages = rawPageFromByteString bsKOps <$> [0, 4096 .. (BS.length bsKOps - 1)]
    -- check pages
    return $ label ("Number of pages: " <> showPowersOf10 (length pages)) $ do
      let vals = concatMap (bifoldMap pure mempty . snd) (WB.content wb)
      tabulate "Value size" (map (showPowersOf10 . sizeofValue) vals) $
        pagesContainEntries bsBlobs pages (WB.content wb)
  where
    getFile fs path =
      FS.withFile fs path FS.ReadMode (fmap BS.toStrict . FS.hGetAll fs)

pagesContainEntries :: ByteString -> [RawPage] ->
                       [(SerialisedKey, Entry SerialisedValue SerialisedBlob)] ->
                       Property
pagesContainEntries _ [] es = counterexample ("k/ops left: " <> show es) (null es)
pagesContainEntries bsBlobs (page : pages) kops
      -- if there's just one key, we have to handle the special case of an overflow
    | [(SerialisedKey key', e)] <- kopsHere
    , LookupEntryOverflow e' suffix <- rawPageLookup page (SerialisedKey key')
    , let appendSuffix (SerialisedValue v)=
                           SerialisedValue
                         . (v <>)
                         . RB.take (fromIntegral suffix)
                         . mconcat
                         . map rawPageRawBytes
                         $ pages
          overflowPages  = (fromIntegral suffix + 4095) `div` 4096
    = classify True "larger-than-page value" $
          e === bimap appendSuffix (readBlob bsBlobs) e'
     .&&. pagesContainEntries bsBlobs (drop overflowPages pages) kopsRest

    | otherwise
    = classify False "larger-than-page value" $
         [ LookupEntry e | (_k, e) <- kopsHere ]
       === [ fmap (second (readBlob bsBlobs))
                  (rawPageLookup page (SerialisedKey k))
           | (SerialisedKey k, _) <- kopsHere ]
      .&&. pagesContainEntries bsBlobs pages kopsRest
  where
    (kopsHere, kopsRest) = splitAt (fromIntegral (rawPageNumKeys page)) kops

-- | Runs in IO, but using a mock file system.
--
-- TODO: Also test file system errors.
prop_WriteAndLoad :: WriteBuffer KeyForIndexCompact LargeRawBytes SerialisedBlob -> Property
prop_WriteAndLoad wb = ioProperty $ do
    fs <- FsSim.mkSimErrorHasFS' FsSim.empty FsSim.emptyErrors
    -- flush write buffer
    let fsPaths = RunFsPaths 1337
    written <- fromWriteBuffer fs fsPaths wb
    loaded <- openFromDisk fs fsPaths

    (1 @=?) =<< readIORef (lsmRunRefCount written)
    (1 @=?) =<< readIORef (lsmRunRefCount loaded)

    lsmRunNumEntries written @=? lsmRunNumEntries loaded
    lsmRunFilter written @=? lsmRunFilter loaded
    lsmRunIndex written @=? lsmRunIndex loaded

    assertEqual "k/ops file"
      (FS.handlePath (lsmRunKOpsFile written))
      (FS.handlePath (lsmRunKOpsFile loaded))
    assertEqual "blob file"
      (FS.handlePath (lsmRunBlobFile written))
      (FS.handlePath (lsmRunBlobFile loaded))

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

rawPageFromByteString :: ByteString -> Int -> RawPage
rawPageFromByteString bs off =
    makeRawPage (toBA bs) off
  where
    -- ensure that the resulting RawPage has no trailing data that could
    -- accidentally be read.
    toBA = (\(SBS.SBS ba) -> BA.ByteArray ba) . SBS.toShort . BS.take (off+4096)

readBlob :: ByteString -> BlobSpan -> SerialisedBlob
readBlob bs (BlobSpan offset size) =
    serialiseBlob $ BS.take (fromIntegral size) (BS.drop (fromIntegral offset) bs)
