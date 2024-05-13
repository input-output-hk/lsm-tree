{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{- HLINT ignore "Eta reduce" -}

module Bench.Database.LSMTree.Internal.IndexCompact (
    benchmarks
    -- * Benchmarked functions
  , searches
  , constructIndexCompact
  ) where

import           Control.DeepSeq (deepseq)
import           Control.Monad.ST (runST)
import           Criterion.Main
import           Data.Foldable (Foldable (..))
import           Database.LSMTree.Extras.Generators
import           Database.LSMTree.Extras.Random
import           Database.LSMTree.Extras.UTxO
import           Database.LSMTree.Internal.IndexCompact
import           Database.LSMTree.Internal.IndexCompactAcc
import           Database.LSMTree.Internal.Serialise (SerialisedKey,
                     serialiseKey)
import           System.Random
import           Test.QuickCheck (generate)

-- See 'utxoNumPages'.
benchmarks :: Benchmark
benchmarks = bgroup "Bench.Database.LSMTree.Internal.IndexCompact" [
      bgroup "searches" [
          env (searchEnv 0  2_500_000 1_000_000) $ \ ~(ic, ks) ->
            bench "searches with 0-bit  rfprec" $ whnf (searches ic) ks
        , env (searchEnv 16 2_500_000 1_000_000) $ \ ~(ic, ks) ->
            bench "searches with 16-bit rfprec" $ whnf (searches ic) ks
        ]
    , bgroup "construction" [
          env (constructionEnv 0  2_500_000) $ \ pages ->
            bench "construction with 0-bit  rfprec and chunk size 100" $ whnf (constructIndexCompact 100) pages
        , env (constructionEnv 16 2_500_000) $ \ pages ->
            bench "construction with 16-bit rfprec and chunk size 100" $ whnf (constructIndexCompact 100) pages
        ]
    ]

-- | Input environment for benchmarking 'searches'.
searchEnv ::
     RFP -- ^ Range-finder bit-precision
  -> Int -- ^ Number of pages
  -> Int -- ^ Number of searches
  -> IO (IndexCompact, [SerialisedKey])
searchEnv rfprec npages nsearches = do
    ic <- constructIndexCompact 100 <$> constructionEnv rfprec npages
    stdgen  <- newStdGen
    let ks = serialiseKey <$> uniformWithReplacement @UTxOKey stdgen nsearches
    pure (ic, ks)

-- | Used for benchmarking 'search'.
searches ::
     IndexCompact
  -> [SerialisedKey]            -- ^ Keys to search for
  -> ()
searches ic ks = foldl' (\acc k -> search k ic `deepseq` acc) () ks

-- | Input environment for benchmarking 'constructIndexCompact'.
constructionEnv ::
     RFP -- ^ Range-finder bit-precision
  -> Int -- ^ Number of pages
  -> IO (RFP, [Append])
constructionEnv rfprec n = do
    stdgen <- newStdGen
    let ks = uniformWithoutReplacement @UTxOKey stdgen (2 * n)
    ps <- generate (mkPages 0 (error "unused in constructionEnv") rfprec ks)
    pure (rfprec, toAppends ps)

-- | Used for benchmarking the incremental construction of a 'IndexCompact'.
constructIndexCompact ::
     ChunkSize
  -> (RFP, [Append]) -- ^ Pages to add in succession
  -> IndexCompact
constructIndexCompact csize (rfprec, apps) = runST $ do
    ica <- new rfprec csize
    mapM_ (`append` ica) apps
    (_, index) <- unsafeEnd ica
    pure index
