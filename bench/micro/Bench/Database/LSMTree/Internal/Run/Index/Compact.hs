{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{- HLINT ignore "Eta reduce" -}

module Bench.Database.LSMTree.Internal.Run.Index.Compact (
    benchmarks
    -- * Benchmarked functions
  , searches
  ) where

import           Control.DeepSeq (deepseq)
import           Criterion.Main
import           Data.Foldable (Foldable (..))
import           Database.LSMTree.Generators
import           Database.LSMTree.Internal.Run.Index.Compact
import           Database.LSMTree.Internal.Serialise (Serialise (serialise),
                     SerialisedKey)
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
  -> IO (CompactIndex, [SerialisedKey])
searchEnv rfprec npages nsearches = do
    ci <- constructCompactIndex 100 <$> constructionEnv rfprec npages
    stdgen  <- newStdGen
    let ks = serialise <$> uniformWithReplacement @UTxOKey stdgen nsearches
    pure (ci, ks)

-- | Used for benchmarking 'search'.
searches ::
     CompactIndex
  -> [SerialisedKey]            -- ^ Keys to search for
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
    pure (rfprec, fromPages (getPages ps))

-- | Used for benchmarking the incremental construction of a 'CompactIndex'.
constructCompactIndex ::
     ChunkSize
  -> (RFPrecision, [Append]) -- ^ Pages to add in succession
  -> CompactIndex
constructCompactIndex (ChunkSize csize) (RFPrecision rfprec, ps) =
    -- under the hood, 'fromList' uses the incremental construction interface
    fromList rfprec csize ps
