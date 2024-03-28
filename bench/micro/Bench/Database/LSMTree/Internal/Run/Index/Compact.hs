{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{- HLINT ignore "Eta reduce" -}

module Bench.Database.LSMTree.Internal.Run.Index.Compact (
    benchmarks
    -- * Benchmarked functions
  , searches
  , constructCompactIndex
  ) where

import           Control.DeepSeq (deepseq)
import           Control.Monad.ST (runST)
import           Criterion.Main
import           Data.Foldable (Foldable (..))
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.LSMTree.Generators
import           Database.LSMTree.Internal.Run.Index.Compact
import           Database.LSMTree.Internal.Run.Index.Compact.Construction
import           Database.LSMTree.Internal.Serialise (SerialisedKey,
                     serialiseKey)
import           System.Random
import           System.Random.Extras
import           Test.QuickCheck (generate)

-- See 'utxoNumPages'.
benchmarks :: Benchmark
benchmarks = bgroup "Bench.Database.LSMTree.Internal.Run.Index.Compact" [
      bgroup "searches" [
          env (searchEnv 0  2_500_000 1_000_000) $ \ ~(ci, ks) ->
            bench "searches with 0-bit  rfprec" $ whnf (searches ci) ks
        , env (searchEnv 16 2_500_000 1_000_000) $ \ ~(ci, ks) ->
            bench "searches with 16-bit rfprec" $ whnf (searches ci) ks
        ]
    , bgroup "construction" [
          env (constructionEnv 0  2_500_000) $ \ pages ->
            bench "construction with 0-bit  rfprec and chunk size 100" $ whnf (constructCompactIndex 100) pages
        , env (constructionEnv 16 2_500_000) $ \ pages ->
            bench "construction with 16-bit rfprec and chunk size 100" $ whnf (constructCompactIndex 100) pages
        ]
    ]

-- | Input environment for benchmarking 'searches'.
searchEnv ::
     RFPrecision -- ^ Range-finder bit-precision
  -> Int         -- ^ Number of pages
  -> Int         -- ^ Number of searches
  -> IO (CompactIndex, Vector SerialisedKey)
searchEnv rfprec npages nsearches = do
    ci <- constructCompactIndex 100 <$> constructionEnv rfprec npages
    stdgen  <- newStdGen
    let ks = serialiseKey <$> uniformWithReplacement @UTxOKey stdgen nsearches
    pure (ci, V.fromList ks)

-- | Used for benchmarking 'search'.
searches ::
     CompactIndex
  -> Vector SerialisedKey            -- ^ Keys to search for
  -> ()
searches ci ks = foldl' (\acc k -> search k ci `deepseq` acc) () ks

-- | Input environment for benchmarking 'constructCompactIndex'.
constructionEnv ::
     RFPrecision -- ^ Range-finder bit-precision
  -> Int         -- ^ Number of pages
  -> IO (RFPrecision, [Append])
constructionEnv rfprec n = do
    stdgen <- newStdGen
    let ks = uniformWithoutReplacement @UTxOKey stdgen (2 * n)
    ps <- generate (mkPages 0 (error "unused in constructionEnv") rfprec ks)
    pure (rfprec, toAppends ps)

-- | Used for benchmarking the incremental construction of a 'CompactIndex'.
constructCompactIndex ::
     ChunkSize
  -> (RFPrecision, [Append]) -- ^ Pages to add in succession
  -> CompactIndex
constructCompactIndex (ChunkSize csize) (RFPrecision rfprec, apps) = runST $ do
    mci <- new rfprec csize
    mapM_ (`append` mci) apps
    (_, index) <- unsafeEnd mci
    pure index
