{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Eta reduce" -}

module Test.Database.LSMTree.Internal.Run.Index.Compact (tests) where

import           Control.Monad (foldM)
import           Control.Monad.State.Strict (MonadState (..), State, evalState,
                     get, put)
import           Data.Bit (Bit (..))
import qualified Data.Bit as BV
import           Data.Coerce (coerce)
import           Data.Foldable (Foldable (..))
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import           Database.LSMTree.Generators as Gen
import           Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise
import           Database.LSMTree.Util
import           Prelude hiding (max, min, pi)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty (TestTree, adjustOption, testGroup)
import           Test.Tasty.QuickCheck (QuickCheckMaxSize (..), testProperty)
import           Test.Util.Orphans ()
import           Text.Printf (printf)

tests :: TestTree
tests = adjustOption (const $ QuickCheckMaxSize 5000) $
        testGroup "Test.Database.LSMTree.Internal.Run.Index.Compact" [
    testGroup "Contruction, searching, chunking" [
        testGroup "prop_searchMinMaxKeysAfterConstruction" [
            testProperty "Full range of UTxOKeys" $
              prop_searchMinMaxKeysAfterConstruction @(WithSerialised UTxOKey) 100
          , testProperty "Small UTxOKeys" $ -- clashes are more likely here
              prop_searchMinMaxKeysAfterConstruction @(WithSerialised (Small UTxOKey)) 100
          , testProperty "Full range of UTxOKeys, optimal range-finder precision" $
              prop_searchMinMaxKeysAfterConstruction @(WithSerialised UTxOKey) 100 . optimiseRFPrecision
          ]
      , testGroup "prop_differentChunkSizesSameResults" [
            testProperty "Full range of UTxOKeys" $
              prop_differentChunkSizesSameResults @(WithSerialised UTxOKey)
          , testProperty "Small UTxOKeys" $
              prop_differentChunkSizesSameResults @(WithSerialised (Small UTxOKey))
          , testProperty "Full range of UTxOKeys, optimal range-finder precision" $
              prop_differentChunkSizesSameResults @(WithSerialised UTxOKey) **. optimiseRFPrecision
          ]
      , testGroup "prop_singlesEquivMulti" [
            testProperty "Full range of UTxOKeys" $
              prop_singlesEquivMulti @(WithSerialised UTxOKey)
          , testProperty "Small UTxOKeys" $
              prop_singlesEquivMulti @(WithSerialised (Small UTxOKey))
          , testProperty "Full range of UTxOKeys, optimal range-finder precision" $
              prop_singlesEquivMulti @(WithSerialised UTxOKey) **. optimiseRFPrecision
          ]
      , testGroup "prop_distribution" [
            testProperty "Full range of UTxOKeys" $
              prop_distribution @(WithSerialised UTxOKey)
          , testProperty "Small UTxOKeys" $
              prop_distribution @(WithSerialised (Small UTxOKey))
          , testProperty "Full range of UTxOKeys, optimal range-finder precision" $
              prop_distribution @(WithSerialised UTxOKey) . optimiseRFPrecision
          ]
      ]
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

deriving instance Eq CompactIndex
deriving instance Show CompactIndex

--
-- Search
--

type CounterM a = State PageNo a

evalCounterM :: CounterM a -> PageNo -> a
evalCounterM = evalState

incrCounter :: CounterM PageNo
incrCounter = get >>= \c -> put (c+1) >> pure c

plusCounter :: PageNo -> CounterM PageNo
plusCounter n = get >>= \c -> put (c+n) >> pure c

-- | After construction, searching for the minimum/maximum key of every page
-- @pageNr@ returns the @pageNr@.
prop_searchMinMaxKeysAfterConstruction ::
     forall k. (SerialiseKey k, Show k, Ord k)
  => ChunkSize
  -> LogicalPageSummaries k
  -> Property
prop_searchMinMaxKeysAfterConstruction csize ps = eqMapProp real model
  where
    model = evalCounterM (foldM modelSearch Map.empty $ getPages ps) 0

    modelSearch :: Map k SearchResult -> LogicalPageSummary k -> CounterM (Map k SearchResult)
    modelSearch m = \case
        OnePageOneKey k       -> do
          c <- incrCounter
          pure $ Map.insert k (SinglePage c) m
        OnePageManyKeys k1 k2 -> do
          c <- incrCounter
          pure $ Map.insert k1 (SinglePage c) $ Map.insert k2 (SinglePage c) m
        MultiPageOneKey k n -> do
          let incr = 1 + fromIntegral n
          c <- plusCounter incr
          pure $ if incr == 1 then Map.insert k (SinglePage c) m
                              else Map.insert k (MultiPage c (c + fromIntegral n)) m

    real = foldMap' realSearch (getPages ps)

    rfprec = coerce $ getRangeFinderPrecision ps
    ci = fromList rfprec (coerce csize) (toAppends ps)

    realSearch :: LogicalPageSummary k -> Map k SearchResult
    realSearch = \case
        OnePageOneKey k           -> Map.singleton k (search (serialiseKey k) ci)
        OnePageManyKeys k1 k2     -> Map.fromList [ (k1, search (serialiseKey k1) ci)
                                                  , (k2, search (serialiseKey k2) ci)]
        MultiPageOneKey k _       -> Map.singleton k (search (serialiseKey k) ci)

--
-- Construction
--

prop_differentChunkSizesSameResults ::
     SerialiseKey k
  => ChunkSize
  -> ChunkSize
  -> LogicalPageSummaries k
  -> Property
prop_differentChunkSizesSameResults
  (ChunkSize csize1)
  (ChunkSize csize2)
  ps = ci1 === ci2
  where
    apps = toAppends ps
    rfprec = coerce $ getRangeFinderPrecision ps
    ci1 = fromList rfprec csize1 apps
    ci2 = fromList rfprec csize2 apps

-- | Constructing an index using only 'appendSingle' is equivalent to using a
-- mix of 'appendSingle' and 'appendMulti'.
prop_singlesEquivMulti ::
     SerialiseKey k
  => ChunkSize
  -> ChunkSize
  -> LogicalPageSummaries k
  -> Property
prop_singlesEquivMulti csize1 csize2 ps = ci1 === ci2
  where
    apps1 = toAppends ps
    apps2 = concatMap toSingles apps1
    rfprec = coerce $ getRangeFinderPrecision ps
    ci1 = fromList rfprec (coerce csize1) apps1
    ci2 = fromListSingles rfprec (coerce csize2) apps2

    toSingles :: Append -> [(SerialisedKey, SerialisedKey)]
    toSingles (AppendSinglePage k1 k2) = [(k1, k2)]
    toSingles (AppendMultiPage k n)    = replicate (fromIntegral n + 1) (k, k)

-- | Distribution of generated test data
prop_distribution :: SerialiseKey k => LogicalPageSummaries k -> Property
prop_distribution ps = labelPages ps $ labelIndex ci $ property True
  where
    apps   = toAppends ps
    rfprec = coerce $ getRangeFinderPrecision ps
    ci     = fromList rfprec 100 apps

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

(**.) :: (a -> b -> d -> e) -> (c -> d) -> a -> b -> c -> e
(**.) f g x1 x2 x3 = f x1 x2 (g x3)

labelIndex :: CompactIndex -> (Property -> Property)
labelIndex ci =
      QC.tabulate "# Clashes" [showPowersOf10 nclashes]
    . QC.tabulate "# Contiguous clash runs" [showPowersOf10 (length nscontig)]
    . QC.tabulate "Length of contiguous clash runs" (fmap (showPowersOf10 . snd) nscontig)
    . QC.tabulate "Contiguous clashes contain multi-page values" (fmap (show . fst) nscontig)
    . QC.classify (multiPageValuesClash ci) "Has clashing multi-page values"
  where nclashes       = Index.countClashes ci
        nscontig       = countContiguousClashes ci

multiPageValuesClash :: CompactIndex -> Bool
multiPageValuesClash ci
    | V.length (ciClashes ci) < 3 = False
    | otherwise                   = V.any p $ V.zip4 v1 v2 v3 v4
  where
    -- note: @i = j - 1@ and @k = j + 1@. This gives us a local view of a
    -- triplet of contiguous LTP bits, and a C bit corresponding to the middle
    -- of the triplet.
    p (cj, ltpi, ltpj, ltpk) =
          -- two multi-page values border eachother
         unBit ltpi && not (unBit ltpj) && unBit ltpk
         -- and they clash
      && unBit cj
    v1 = V.tail (ciClashes ci)
    v2 = ciLargerThanPage ci
    v3 = V.tail v2
    v4 = V.tail v3

-- Returns the number of entries and whether any multi-page values were in the
-- contiguous clashes
countContiguousClashes :: CompactIndex -> [(Bool, Int)]
countContiguousClashes ci = actualContigClashes
  where
    -- filtered is a list of maximal sub-vectors that have only only contiguous
    -- clashes
    zipped    = V.zip (ciClashes ci) (ciLargerThanPage ci)
    grouped   = V.groupBy (\x y -> fst x == fst y) zipped
    filtered  = filter (V.all (\(c, _ltp) -> c == Bit True)) grouped
    -- clashes that are part of a multi-page value shouldn't be counted towards
    -- the total number of /actual/ clashes. We only care about /actual/ clashes
    -- if they total more than 1 (otherwise it's just a single clash)
    actualContigClashes = filter (\(_, n) -> n > 1) $
                          fmap (\v -> (countLTP v > 0, countC v - countLTP v)) filtered
    countC              = V.length
    countLTP            = BV.countBits . V.map snd

-- | Point-wise equality test for two maps, returning counterexamples for each
-- mismatch.
eqMapProp :: (Ord k, Eq v, Show k, Show v) => Map k v -> Map k v -> Property
eqMapProp m1 m2 = conjoin . Map.elems $
    Merge.merge
      (Merge.mapMissing onlyLeft)
      (Merge.mapMissing onlyRight)
      (Merge.zipWithMatched both)
      m1
      m2
  where
    onlyLeft k x = flip counterexample (property False) $
      printf "Key-value pair only on the left, (k, x) = (%s, %s)" (show k) (show x)
    onlyRight k y = flip counterexample (property False) $
      printf "Key-value pair only on the right, (k, y) = (%s, %s)" (show k) (show y)
    both k x y = flip counterexample (property (x == y)) $
      printf "Mismatch between x and y, (k, x, y) = (%s, %s, %s)" (show k) (show x) (show y)
