{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Database.LSMTree.Internal.Run.Index.Compact (tests) where

import           Control.Monad.State.Strict
import           Data.Foldable (Foldable (foldMap'))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Database.LSMTree.Generators as Gen (ChunkSize (..), Page (..),
                     Pages (..), RFPrecision (..), UTxOKey (..),
                     WithSerialised (..), fromPages, labelPages)
import           Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise (Serialise (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, adjustOption, testGroup)
import           Test.Tasty.QuickCheck (QuickCheckMaxSize (..), testProperty)

tests :: TestTree
tests = testGroup "Test.Database.LSMTree.Internal.Run.Index.Compact" [
      -- Increasing the maximum size has the effect of generating more
      -- interesting numbers of partitioned pages. With a max size of 100, the
      -- tests are very likely to generate only 1 partitioned page.
      adjustOption (const $ QuickCheckMaxSize 5000) $
      testGroup "Contruction, searching, chunking" [
        testProperty "prop_searchMinMaxKeysAfterConstruction" $
          prop_searchMinMaxKeysAfterConstruction @(WithSerialised UTxOKey)
      , testProperty "prop_differentChunkSizesSameResults" $
          prop_differentChunkSizesSameResults @(WithSerialised UTxOKey)
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

type CounterM a = State Int a

evalCounterM :: CounterM a -> Int -> a
evalCounterM = evalState

incrCounter :: CounterM Int
incrCounter = get >>= \c -> put (c+1) >> pure c

plusCounter :: Int -> CounterM Int
plusCounter n = get >>= \c -> put (c+n) >> pure c

-- | After construction, searching for the minimum/maximum key of every page
-- @pageNr@ returns the @pageNr@.
prop_searchMinMaxKeysAfterConstruction ::
     forall k. (Serialise k, Show k, Ord k)
  => ChunkSize
  -> Pages k
  -> Property
prop_searchMinMaxKeysAfterConstruction
  (ChunkSize csize)
  ps0@(Pages (RFPrecision rfprec) ps1) =
      classify (hasClashes ci) "Compact index contains clashes"
    $ labelPages ps0
    $ counterexample (show ci)
    $ real === model
  where
    model = evalCounterM (modelSearches ps1) 0

    modelSearches :: [Gen.Page k] -> CounterM (Map k SearchResult)
    modelSearches []     = pure Map.empty
    modelSearches (p:ps) = case p of
        Gen.OneKey k       -> do
          c <- incrCounter
          Map.insert k (SinglePage c) <$> modelSearches ps
        Gen.ManyKeys k1 k2 -> do
          c <- incrCounter
          Map.insert k1 (SinglePage c) . Map.insert k2 (SinglePage c) <$> modelSearches ps
        Gen.LargerThanPage k n -> do
          let incr = 1 + fromIntegral n
          c <- plusCounter incr
          if incr == 1 then
            Map.insert k (SinglePage c) <$> modelSearches ps
          else
            Map.insert k (MultiPage c (c + fromIntegral n)) <$> modelSearches ps

    real = foldMap' realSearch ps1

    ci = fromList rfprec csize (fromPages ps1)

    realSearch :: Gen.Page k -> Map k SearchResult
    realSearch = \case
        Gen.OneKey k           -> Map.singleton k (search (serialise k) ci)
        Gen.ManyKeys k1 k2     -> Map.fromList [ (k1, search (serialise k1) ci)
                                               , (k2, search (serialise k2) ci)]
        Gen.LargerThanPage k _ -> Map.singleton k (search (serialise k) ci)

--
-- Construction
--

prop_differentChunkSizesSameResults ::
     Serialise k
  => ChunkSize
  -> ChunkSize
  -> Pages k
  -> Property
prop_differentChunkSizesSameResults
  (ChunkSize csize1)
  (ChunkSize csize2)
  pps@(Pages (RFPrecision rfprec) ps) =
      labelPages pps
    $ ci1 === ci2
  where
    ps' = fromPages ps
    ci1 = fromList rfprec csize1 ps'
    ci2 = fromList rfprec csize2 ps'
