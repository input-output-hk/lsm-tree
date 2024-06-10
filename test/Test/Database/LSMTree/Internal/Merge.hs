{-# LANGUAGE LambdaCase #-}

module Test.Database.LSMTree.Internal.Merge (tests) where

import           Data.Bifoldable (bifoldMap)
import qualified Data.BloomFilter as Bloom
import           Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Word (Word64)
import           Database.LSMTree.Extras
import           Database.LSMTree.Extras.Generators (KeyForIndexCompact,
                     TypedWriteBuffer (..))
import qualified Database.LSMTree.Internal.Entry as Entry
import qualified Database.LSMTree.Internal.Merge as Merge
import           Database.LSMTree.Internal.Paths (RunFsPaths (..),
                     pathsForRunFiles)
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.Serialise
import           Database.LSMTree.Internal.WriteBuffer (WriteBuffer)
import qualified Database.LSMTree.Internal.WriteBuffer as WB
import qualified System.FS.API as FS
import qualified System.FS.API.Lazy as FS
import qualified System.FS.BlockIO.API as FS
import           Test.Database.LSMTree.Internal.Run (isLargeKOp, readKOps)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.FS (withTempIOHasBlockIO)

tests :: TestTree
tests = testGroup "Test.Database.LSMTree.Internal.Merge"
    [ testProperty "prop_MergeDistributes" $ \level stepSize wbs ->
        ioPropertyWithRealFS $ \fs hbio ->
          prop_MergeDistributes fs hbio level stepSize wbs
    , testProperty "prop_CloseMerge" $ \level stepSize wbs ->
        ioPropertyWithRealFS $ \fs hbio ->
          prop_CloseMerge fs hbio level stepSize wbs
    ]
  where
    -- TODO: run using mock file system once simulation is merged:
    -- https://github.com/input-output-hk/fs-sim/pull/48
    -- (also check all handles closed, see Test.Database.LSMTree.Internal.Run)
    ioPropertyWithRealFS prop = ioProperty $ withTempIOHasBlockIO "session-merge" prop

-- | Creating multiple runs from write buffers and merging them leads to the
-- same run as merging the write buffers and creating a run.
--
-- @mergeRuns . map flush === flush . mergeWriteBuffers@
prop_MergeDistributes ::
     FS.HasFS IO h ->
     FS.HasBlockIO IO h ->
     Merge.Level ->
     StepSize ->
     [TypedWriteBuffer KeyForIndexCompact SerialisedValue SerialisedBlob] ->
     IO Property
prop_MergeDistributes fs hbio level stepSize (fmap unTypedWriteBuffer -> wbs) = do
    runs <- sequenceA $ zipWith flush [10..] wbs
    let stepsNeeded = sum (map (Entry.unNumEntries . WB.numEntries) wbs)
    (stepsDone, lhs) <- mergeRuns fs hbio level 0 runs stepSize

    rhs <- flush 1 (mergeWriteBuffers level wbs)

    -- TODO: buffers must be 4K aligned
    lhsKOpsFile <- FS.hGetAll fs (Run.runKOpsFile lhs)
    lhsBlobFile <- FS.hGetAll fs (Run.runBlobFile lhs)
    rhsKOpsFile <- FS.hGetAll fs (Run.runKOpsFile rhs)
    rhsBlobFile <- FS.hGetAll fs (Run.runBlobFile rhs)

    lhsKOps <- readKOps fs lhs
    rhsKOps <- readKOps fs rhs

    -- cleanup
    traverse_ (Run.removeReference fs) runs
    Run.removeReference fs lhs
    Run.removeReference fs rhs

    return $ stats $
           counterexample "numEntries"
           (Run.runNumEntries lhs === Run.runNumEntries rhs)
      .&&. -- we can't just test bloom filter equality, their sizes may differ.
           counterexample "runFilter"
           (Bloom.length (Run.runFilter lhs) >= Bloom.length (Run.runFilter rhs))
      .&&. -- the index is equal, but only because range finder precision is
           -- always 0 for the numbers of entries we are dealing with.
           counterexample "runIndex"
           (Run.runIndex lhs === Run.runIndex rhs)
      .&&. counterexample "kops"
           (lhsKOps === rhsKOps)
      .&&. counterexample "kopsFile"
           (lhsKOpsFile === rhsKOpsFile)
      .&&. counterexample "blobFile"
           (lhsBlobFile === rhsBlobFile)
      .&&. counterexample ("step counting")
           (stepsDone === stepsNeeded)
  where
    flush n = Run.fromWriteBuffer fs hbio (RunFsPaths (FS.mkFsPath []) n)

    stats = tabulate "value size" (map (showPowersOf10 . sizeofValue) vals)
          . label (if any isLargeKOp kops then "has large k/op" else "no large k/op")
          . label ("number of runs: " <> showPowersOf10 (length wbs))
    kops = foldMap WB.toList wbs
    vals = concatMap (bifoldMap pure mempty . snd) kops

-- | After merging for a few steps, we can prematurely abort the merge, which
-- should clean up properly.
prop_CloseMerge ::
     FS.HasFS IO h ->
     FS.HasBlockIO IO h ->
     Merge.Level ->
     StepSize ->
     [TypedWriteBuffer KeyForIndexCompact SerialisedValue SerialisedBlob] ->
     IO Property
prop_CloseMerge fs hbio level (Positive stepSize) (fmap unTypedWriteBuffer -> wbs) = do
    let path0 = RunFsPaths (FS.mkFsPath []) 0
    runs <- sequenceA $ zipWith flush [10..] wbs
    mergeToClose <- makeInProgressMerge path0 runs
    traverse_ (Merge.close fs) mergeToClose

    filesExist <- traverse (FS.doesFileExist fs) (pathsForRunFiles path0)

    -- cleanup
    traverse_ (Run.removeReference fs) runs

    return $
      counterexample ("run files exist: " <> show filesExist) $
        isJust mergeToClose ==> all not filesExist
  where
    flush n = Run.fromWriteBuffer fs hbio (RunFsPaths (FS.mkFsPath []) n)

    makeInProgressMerge path runs =
      Merge.new fs level mappendValues path runs >>= \case
        Nothing -> return Nothing  -- not in progress
        Just merge -> do
          -- just do a few steps once, ideally not completing the merge
          Merge.steps fs hbio merge stepSize >>= \case
            (_, Merge.MergeComplete run) -> do
              Run.removeReference fs run  -- run not needed, close
              return Nothing  -- not in progress
            (_, Merge.MergeInProgress) ->
              return (Just merge)

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

type StepSize = Positive Int

mergeRuns ::
     FS.HasFS IO h ->
     FS.HasBlockIO IO h ->
     Merge.Level ->
     Word64 ->
     [Run.Run (FS.Handle h)] ->
     StepSize ->
     IO (Int, Run.Run (FS.Handle h))
mergeRuns fs hbio level runNumber runs (Positive stepSize) = do
    Merge.new fs level mappendValues (RunFsPaths (FS.mkFsPath []) runNumber) runs >>= \case
      Nothing -> (,) 0 <$> Run.fromWriteBuffer fs hbio (RunFsPaths (FS.mkFsPath []) runNumber) WB.empty
      Just m  -> go 0 m
  where
    go !steps m =
        Merge.steps fs hbio m stepSize >>= \case
          (n, Merge.MergeComplete run) -> return (steps + n, run)
          (n, Merge.MergeInProgress)   -> go (steps + n) m

mergeWriteBuffers :: Merge.Level -> [WriteBuffer] -> WriteBuffer
mergeWriteBuffers level =
    WB.fromMap
      . (if level == Merge.LastLevel then Map.filter (not . isDelete) else id)
      . Map.unionsWith (Entry.combine mappendValues)
      . map WB.toMap
  where
    isDelete Entry.Delete = True
    isDelete _            = False

mappendValues :: SerialisedValue -> SerialisedValue -> SerialisedValue
mappendValues (SerialisedValue x) (SerialisedValue y) = SerialisedValue (x <> y)
