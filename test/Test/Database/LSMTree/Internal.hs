{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: generalise tests in this module for IOSim, not just IO. Do this once we
-- add proper support for IOSim for fault testing.
module Test.Database.LSMTree.Internal (tests) where

import           Control.Concurrent.Class.MonadMVar.Strict
import           Control.Exception
import           Control.Monad (void)
import           Data.Bifunctor
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Sum (..))
import qualified Data.Vector as V
import           Data.Word (Word64)
import           Database.LSMTree.Extras (showPowersOf)
import           Database.LSMTree.Internal
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.Paths (mkSnapshotName)
import           Database.LSMTree.Internal.Serialise
import qualified System.FS.API as FS
import qualified Test.Database.LSMTree.Internal.Lookup as Test
import           Test.Database.LSMTree.Internal.Lookup
                     (InMemLookupData (runData))
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Util.FS

tests :: TestTree
tests = testGroup "Test.Database.LSMTree.Internal" [
      testCase "newSession" newSession
    , testCase "restoreSession" restoreSession
    , testCase "twiceOpenSession" twiceOpenSession
    , testCase "sessionDirLayoutMismatch" sessionDirLayoutMismatch
    , testCase "sessionDirDoesNotExist" sessionDirDoesNotExist
    , testProperty "prop_interimRestoreSessionUniqueRunNames"
        prop_interimRestoreSessionUniqueRunNames
    , testProperty "prop_interimOpenTable" prop_interimOpenTable
    ]

newSession :: Assertion
newSession = withTempIOHasBlockIO "newSession" $ \hfs hbio ->
    void $ openSession hfs hbio (FS.mkFsPath [])

restoreSession :: Assertion
restoreSession = withTempIOHasBlockIO "restoreSession" $ \hfs hbio -> do
    session1 <- openSession hfs hbio (FS.mkFsPath [])
    closeSession session1
    void $ openSession hfs hbio (FS.mkFsPath [])

twiceOpenSession :: Assertion
twiceOpenSession = withTempIOHasBlockIO "twiceOpenSession" $ \hfs hbio -> do
    void $ openSession hfs hbio (FS.mkFsPath [])
    try @LSMTreeError (openSession hfs hbio (FS.mkFsPath [])) >>= \case
      Left (SessionDirLocked _) -> pure ()
      x -> assertFailure $ "Opening a session twice in the same directory \
                           \should fail with an SessionDirLocked error, but \
                           \it returned this instead: " <> showLeft "Session" x

sessionDirLayoutMismatch :: Assertion
sessionDirLayoutMismatch = withTempIOHasBlockIO "sessionDirLayoutMismatch" $ \hfs hbio -> do
    FS.createDirectory hfs (FS.mkFsPath ["unexpected-directory"])
    try @LSMTreeError (openSession hfs hbio (FS.mkFsPath [])) >>= \case
      Left (SessionDirMalformed _) -> pure ()
      x -> assertFailure $ "Restoring a session in a directory with a wrong \
                           \layout should fail with a SessionDirMalformed, but \
                           \it returned this instead: " <> showLeft "Session" x

sessionDirDoesNotExist :: Assertion
sessionDirDoesNotExist = withTempIOHasBlockIO "sessionDirDoesNotExist" $ \hfs hbio -> do
    try @LSMTreeError (openSession hfs hbio (FS.mkFsPath ["missing-dir"])) >>= \case
      Left (SessionDirDoesNotExist _) -> pure ()
      x -> assertFailure $ "Opening a session in a non-existent directory should \
                           \fail with a SessionDirDoesNotExist error, but it \
                           \returned this instead: " <> showLeft "Session" x

showLeft :: Show a => String -> Either a b -> String
showLeft x = \case
    Left e -> show e
    Right _ -> x

-- | Runs are currently not deleted when they become unreferenced. As such, when
-- a session is restored, there are still runs in the active directory. When we
-- restore a session, we must ensure that we do not use names for new runs that
-- are already used for existing runs. As such, we should set the
-- @sessionUniqCounter@ accordingly, such that it starts at a number strictly
-- larger then numbers of the runs in the active directory.
--
-- TODO: remove once we have proper snapshotting, in which case files in the
-- active directory are deleted when a session is restored: loading snapshots is
-- the only way to get active runs into the active directory.
prop_interimRestoreSessionUniqueRunNames ::
     Positive (Small Int)
  -> NonNegative Int
  -> Property
prop_interimRestoreSessionUniqueRunNames (Positive (Small n)) (NonNegative m) = ioProperty $
    withTempIOHasBlockIO "TODO" $ \hfs hbio -> do
      prop1 <- withSession hfs hbio (FS.mkFsPath []) $ \sesh -> do
        withTable sesh conf $ \th -> do
          updates upds th
          withOpenTable th $ \thEnv -> do
            tc <- readMVar (tableContent thEnv)
            let (Sum nruns) = V.foldMap
                                (V.foldMap (const (Sum (1 :: Int))) . residentRuns)
                                (tableLevels tc)
            pure $ tabulate "number of runs on disk" [showPowersOf 2 nruns]
                $ True

      withSession hfs hbio (FS.mkFsPath []) $ \sesh -> do
        withTable sesh conf $ \th -> do
          eith <- try (updates upds th)
          fmap (prop1 .&&.) $ case eith of
            Left (e :: FS.FsError)
              | FS.fsErrorType e == FS.FsResourceAlreadyExist
              -> pure $ counterexample "Test failed... found an FsResourceAlreadyExist error" False
              | otherwise
              -> throwIO e
            Right () -> pure $ property True
  where
    conf = TableConfig {
        confMergePolicy = MergePolicyLazyLevelling
      , confSizeRatio = Four
        -- Write buffer size is small on purpose, so that the test actually
        -- flushes and merges.
      , confWriteBufferAlloc = AllocNumEntries (NumEntries n)
      , confBloomFilterAlloc = AllocFixed 10
      , confResolveMupsert = Nothing
      }

    upds = V.fromList [ (serialiseKey i, Insert (serialiseValue i))
                      | (i :: Word64) <- fmap fromIntegral [1..m]
                      ]

-- | Check that opening a populated table via the interim table loading function
-- works as expected. Roughly, we test:
--
-- @
--  inserts th kvs == open' (snapshot' (inserts th kvs))
-- @
--
-- TODO: remove once we have proper snapshotting
prop_interimOpenTable ::
     Test.InMemLookupData SerialisedKey SerialisedValue SerialisedBlob
  -> Property
prop_interimOpenTable dat = ioProperty $
    withTempIOHasBlockIO "prop_interimOpenTable" $ \hfs hbio -> do
      withSession hfs hbio (FS.mkFsPath []) $ \sesh -> do
        withTable sesh conf $ \th -> do
          updates upds th
          let snap = fromMaybe (error "invalid name") $ mkSnapshotName "snap"
          numRunsSnapped <- snapshot snap "someLabel" th
          th' <- open sesh "someLabel" snap
          lhs <- lookups ks th id
          rhs <- lookups ks th' id
          close th
          close th'
          -- TODO: checking lookups is a simple check, but we could have stronger
          -- guarantee. For example, we might check that the internal structures
          -- match.
          --
          -- TODO: remove opaqueifyBlobs once blobs are implemented.
          pure $ tabulate "Number of runs snapshotted" [show numRunsSnapped]
              $ (Test.opaqueifyBlobs lhs === Test.opaqueifyBlobs rhs)
  where
    conf = TableConfig {
        confMergePolicy = MergePolicyLazyLevelling
      , confSizeRatio = Four
        -- Write buffer size is small on purpose, so that the test actually
        -- flushes and merges.
      , confWriteBufferAlloc = AllocNumEntries (NumEntries 3)
      , confBloomFilterAlloc = AllocFixed 10
      , confResolveMupsert = Nothing
      }

    Test.InMemLookupData { runData, lookups = keysToLookup } = dat
    -- TODO: include inserts with blobs once blob retrieval is implemented
    runDataWithoutblobs = Map.map f runData
      where f (InsertWithBlob v _) = Insert v
            f x                    = x
    ks = V.map serialiseKey (V.fromList keysToLookup)
    upds = V.fromList $ fmap (bimap serialiseKey (bimap serialiseValue serialiseBlob))
                      $ Map.toList runDataWithoutblobs

{- TODO: add test checking table layout invariants after each update.
   (check size and number of runs at each level)
levelsInvariant :: TableConfig -> Levels h -> Bool
levelsInvariant config = go 1 (fromIntegral (confWriteBufferAlloc config))
  where
    go n maxSizeTiering (V.uncons -> Nothing) =
      True
    go n maxSizeTiering (V.uncons -> Just (Level runs, nextLevels)) =
      case mergePolicyForLevel (confMergePolicy config) n nextLevels of
        LevelTiering ->
             V.length runs < sizeRatioInt (confSizeRatio config)
          && and [runSize r <= maxSizeTiering | r <- runs]
        LevelLevelling ->
             V.
-}
