{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- HLINT ignore "Use const" -}

module Bench.Database.LSMTree.Internal.Integration (benchmarks, analysis) where

import           Bench.Database.LSMTree.Internal.Run.BloomFilter (elems)
import           Bench.Database.LSMTree.Internal.Run.Index.Compact (searches)
import           Control.DeepSeq (NFData)
import           Control.Monad
import           Criterion.Main (Benchmark, bench, bgroup, env, nf, whnf)
import           Data.List (sort)
import           Data.List.Extra (nubSort)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy (..))
import           Database.LSMTree.Generators (RFPrecision (..), UTxOKey)
import           Database.LSMTree.Internal.Integration (prepLookups)
import           Database.LSMTree.Internal.Run.BloomFilter (Bloom)
import qualified Database.LSMTree.Internal.Run.BloomFilter as Bloom
import           Database.LSMTree.Internal.Run.Index.Compact (Append (..),
                     CompactIndex)
import qualified Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise (Serialise (..),
                     SerialisedKey, topBits16)
import           Database.LSMTree.Util.Orphans ()
import           GHC.Generics (Generic)
import           Prelude hiding (getContents)
import           System.Random (Uniform, newStdGen)
import           System.Random.Extras (sampleUniformWithReplacement,
                     uniformWithoutReplacement)
import           Test.QuickCheck (generate, shuffle)
import           Text.Printf (printf)

benchmarks :: Benchmark
benchmarks = bgroup "Bench.Database.LSMTree.Internal.Integration" [
      bgroup "prepLookups for a single run" [
          benchPrepLookups defaultConfig
        , benchPrepLookups (defaultConfig {
              name = "default onlyPos"
            , nneg = 0
            })
        , benchPrepLookups (defaultConfig {
              name = "default onlyNeg"
            , npos = 0
            })
        , benchPrepLookups (defaultConfig {
              name = "default high fpr"
            , fpr  = 0.9
            })
        , benchPrepLookups (defaultConfig {
              name   = "default small"
            , npages = 10_000
            , npos   = 1_000
            , nneg   = 1_000
            })
        ]
    ]
  where
    benchPrepLookups :: Config -> Benchmark
    benchPrepLookups conf@Config{name} =
      env (prepLookupsEnv (Proxy @UTxOKey) conf) $ \ ~(b, ci, ks) ->
        -- see 'npages'.
        bgroup (printf "%s, %d actual pages" name (Index.sizeInPages ci)) [
            -- the bloomfilter is queried for all lookup keys
            bench "Bloomfilter query"    $ whnf (elems b) ks
            -- the compact index is only searched for (true and false) positive
            -- lookup keys
          , env (pure $ filter (`Bloom.elem` b) ks) $ \ks' ->
            bench "Compact index search" $ whnf (searches ci) ks'
            -- All prepped lookups are going to be used eventually so we use
            -- @nf@ on the result of 'prepLookups' to ensure that we actually
            -- compute the full list.
          , bench "In-memory lookup"     $ nf (prepLookups  [((), b, ci)]) ks
          ]

{-------------------------------------------------------------------------------
  Analysis
-------------------------------------------------------------------------------}

-- In this analysis, around @15%@ to @20%@ of the measured time for
-- 'prepLookups' is not accounted for by bloom filter queries and compact index
-- searches.
analysis :: IO ()
analysis = do
      -- (name, bloomfilter query, compact index search, prepLookups)
  let def     = ("default", 1.722 , 0.966   , 3.294)
      onlyPos = ("onlyPos", 0.9108, 0.8873  , 2.139)
      onlyNeg = ("onlyNeg", 0.6784, 0.009573, 0.8683)
      highFpr = ("highFpr", 1.155 , 1.652   , 3.417)
      small   = ("small"  , 0.1602, 0.06589 , 0.2823)

      results :: [(String, Double, Double, Double)]
      results = [def, onlyPos, onlyNeg, highFpr, small]

  forM_ results $ \(name, query, search, prep) -> do
    -- the measured time for 'prepLookups' should be close to the time spent on
    -- bloom filter queries and compact index searches
    let diff        = prep - (query + search)
        diffPercent = diff / prep
    print (name, query, search, prep, diff, diffPercent)

{-------------------------------------------------------------------------------
  Environments
-------------------------------------------------------------------------------}

-- | Config options describing a benchmarking scenario
data Config = Config {
    -- | Name for the benchmark scenario described by this config.
    name         :: !String
    -- | If 'Nothing', use 'suggestRangeFinderPrecision'.
  , rfprecDef    :: !(Maybe Int)
    -- | Chunk size for compact index construction
  , csize        :: !Int
    -- | Number of pages in total
    --
    -- Note: the actual number of pages can be higher, because of the
    -- partitioned pages restriction.
  , npages       :: !Int
    -- | Number of entries per page
  , npageEntries :: !Int
    -- | Number of positive lookups
  , npos         :: !Int
    -- | Number of negative lookups
  , nneg         :: !Int
  , fpr          :: !Double
  }

defaultConfig :: Config
defaultConfig = Config {
    name         = "default"
  , rfprecDef    = Nothing
  , csize        = 100
  , npages       = 50_000
  , npageEntries = 40
  , npos         = 10_000
  , nneg         = 10_000
  , fpr          = 0.1
  }

-- | Use 'lookupsEnv' to set up an environment for the in-memory aspect of
-- lookups.
prepLookupsEnv ::
     forall k. (Ord k, Uniform k, Serialise k)
  => Proxy k
  -> Config
  -> IO (Bloom SerialisedKey, CompactIndex, [SerialisedKey])
prepLookupsEnv _ Config {..} = do
    (storedKeys, lookupKeys) <- lookupsEnv @k totalEntries npos nneg
    let b    = Bloom.fromList fpr $ fmap serialise storedKeys
        ps   = mkPages (RFPrecision rfprec) $ NonEmpty.fromList storedKeys
        ps'  = fmap serialise ps
        ps'' = fromPage <$> getPages ps'
        ci   = Index.fromList rfprec csize ps''
    pure (b, ci, fmap serialise lookupKeys)
  where
    totalEntries = npages * npageEntries
    rfprec = fromMaybe (Index.suggestRangeFinderPrecision npages) rfprecDef

-- | Generate keys to store and keys to lookup
lookupsEnv ::
     (Ord k, Uniform k)
  => Int -- ^ Number of stored keys
  -> Int -- ^ Number of positive lookups
  -> Int -- ^ Number of negative lookups
  -> IO ([k], [k])
lookupsEnv nkeys npos nneg = do
    stdgen <- newStdGen
    stdgen' <- newStdGen
    let (xs, ys1) = splitAt nkeys
                  $ uniformWithoutReplacement    stdgen  (nkeys + nneg)
        ys2       = sampleUniformWithReplacement stdgen' npos xs
    zs <- generate $ shuffle (ys1 ++ ys2)
    pure (xs, zs)

{-------------------------------------------------------------------------------
  Pages
-------------------------------------------------------------------------------}

-- TODO: either remove the @f@ parameter and specialise for 'NonEmpty', or merge
-- this code with the @Pages@ type from "Database.LSMTree.Generators".

pageResidency :: Int
pageResidency = 40

class MinMax f where
  minKey :: Page f k -> k
  maxKey :: Page f k -> k

instance MinMax NonEmpty where
  minKey = NonEmpty.head . getContents
  maxKey = NonEmpty.last . getContents

newtype Page f k = Page { getContents :: f k }
  deriving stock (Show, Generic, Functor)
  deriving anyclass NFData

-- | TODO: enable larger-than-page values
fromPage :: Page NonEmpty SerialisedKey -> Append
fromPage (Page (k :| [])) = AppendSinglePage k k
fromPage (Page (k :| ks)) = AppendSinglePage k (last ks)

{-------------------------------------------------------------------------------
  Pages
-------------------------------------------------------------------------------}

-- | We model a disk page in a run as a pair of its minimum and maximum key.
--
-- A run consists of multiple pages in sorted order, and keys are unique. Pages
-- are partitioned, meaning all keys inside a page have the same range-finder
-- bits. A run can not be empty, and a page can not be empty.
data Pages f k = Pages {
    getRangeFinderPrecision :: RFPrecision
  , getPages                :: [Page f k]
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass NFData

mkPages ::
     forall k. (Ord k, Serialise k)
  => RFPrecision
  -> NonEmpty k
  -> Pages NonEmpty k
mkPages rfprec@(RFPrecision n) =
    Pages rfprec . go . nubSort . NonEmpty.toList
  where
    go :: [k] -> [Page NonEmpty k]
    go []     = []
    go [k]    = [Page $ k :| []]
    go (k:ks) = Page (NonEmpty.fromList (k:ks1)) : go ks2
      where
        (ks1, ks2) = spanN
                (pageResidency - 1)
                (\k' -> topBits16 n (serialise k) == topBits16 n (serialise k'))
                ks

_pagesInvariant :: (Ord k, Serialise k) => Pages NonEmpty k -> Bool
_pagesInvariant (Pages (RFPrecision rfprec) ks) =
       sort ks'   == ks'
    && nubSort ks' == ks'
    && not (null ks)
    && all partitioned ks
  where
    ks' = flatten ks
    partitioned p =
         -- keys should be sorted within pages, so it's sufficient to check
         -- the minimum key against the maximum key
         topBits16 rfprec (serialise $ minKey p)
      == topBits16 rfprec (serialise $ maxKey p)

    flatten :: Eq k => [Page NonEmpty k] -> [k]
    flatten []              = []
    flatten (Page ks'':kss) = NonEmpty.toList ks'' ++ flatten kss

-- | @'spanN' n p xs@ finds the longest prefix of at most length @n@ of @xs@ of
-- elements that satisfy @p@.
--
-- Note: this is a frankenstein fusion of 'span' and 'splitAt', which is
-- hopefully slightly faster than using 'take', 'takeWhile', 'drop' and
-- 'dropWhile' to achieve the same result.
spanN :: Int -> (a -> Bool) -> [a] -> ([a], [a])
spanN n p ls
  | n <= 0    = ([], ls)
  | otherwise = spanN' n ls
  where
    spanN' _ xs@[]      = (xs, xs)
    spanN' m xs@(x:xs') | 0 <- m    = ([], xs)
                        | p x       = let (ys, zs) = spanN' (m-1) xs'
                                      in  (x:ys, zs)
                        | otherwise = ([], xs)
