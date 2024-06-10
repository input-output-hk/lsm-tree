{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bench.Database.LSMTree.Internal.Lookup (benchmarks) where

import           Control.Exception (assert)
import           Control.Monad
import           Control.Monad.ST.Strict (stToIO)
import           Criterion.Main (Benchmark, bench, bgroup, env, envWithCleanup,
                     perRunEnv, whnf, whnfAppIO)
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import           Database.LSMTree.Extras.Orphans ()
import           Database.LSMTree.Extras.Random (frequency,
                     sampleUniformWithReplacement, uniformWithoutReplacement)
import           Database.LSMTree.Extras.UTxO
import           Database.LSMTree.Internal.Entry (Entry (..), NumEntries (..))
import           Database.LSMTree.Internal.Lookup (bloomQueriesDefault,
                     indexSearches, intraPageLookups, lookupsIO, prepLookups)
import           Database.LSMTree.Internal.Paths (RunFsPaths (..))
import           Database.LSMTree.Internal.Run (Run)
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.Serialise
import qualified Database.LSMTree.Internal.WriteBuffer as WB
import           Prelude hiding (getContents)
import           System.Directory (removeDirectoryRecursive)
import qualified System.FS.API as FS
import qualified System.FS.BlockIO.API as FS
import qualified System.FS.BlockIO.IO as FS
import qualified System.FS.IO as FS
import           System.IO.Temp
import           System.Random as R
import           Test.QuickCheck (generate, shuffle)

-- | TODO: add a separate micro-benchmark that includes multi-pages.
benchmarks :: Benchmark
benchmarks = bgroup "Bench.Database.LSMTree.Internal.Lookup" [
      benchLookups Config {
          name         = "2_000_000 entries, 256 positive lookups"
        , nentries     = 2_000_000
        , npos         = 256
        , nneg         = 0
        , ioctxps      = Nothing
        }
    , benchLookups Config {
          name = "2_000_000 entries, 256 negative lookups"
        , nentries = 2_000_000
        , npos = 0
        , nneg = 256
        , ioctxps = Nothing
        }
    ]

benchLookups :: Config -> Benchmark
benchLookups conf@Config{name} =
    withEnv $ \ ~(_dir, _hasFS, hasBlockIO, rs, ks) ->
      env ( pure ( V.map Run.runFilter rs
                 , V.map Run.runIndex rs
                 , V.map Run.runKOpsFile rs
                 )
          ) $ \ ~(blooms, indexes, kopsFiles) ->
        bgroup name [
            -- The bloomfilter is queried for all lookup keys. The result is an
            -- unboxed vector, so only use @whnf@.
            bench "Bloomfilter query" $
              whnf (\ks' -> bloomQueriesDefault blooms ks') ks
            -- The compact index is only searched for (true and false) positive
            -- lookup keys. We use whnf here because the result is
          , env (pure $ bloomQueriesDefault blooms ks) $ \rkixs ->
              bench "Compact index search" $
                whnfAppIO (\ks' -> stToIO $ indexSearches indexes kopsFiles ks' rkixs) ks
            -- prepLookups combines bloom filter querying and index searching.
            -- The implementation forces the results to WHNF, so we use
            -- whnfAppIO here instead of nfAppIO.
          , bench "Lookup preparation in memory" $
              whnfAppIO (\ks' -> stToIO $ prepLookups blooms indexes kopsFiles ks') ks
            -- Submit the IOOps we get from prepLookups to HasBlockIO. We use
            -- perRunEnv because IOOps contain mutable buffers, so we want fresh
            -- ones for each run of the benchmark. We manually evaluate the
            -- result to WHNF since it is unboxed vector.
          , bench "Submit IOOps" $
              perRunEnv (stToIO $ prepLookups blooms indexes kopsFiles ks) $ \ ~(_rkixs, ioops) -> do
                !_ioress <- FS.submitIO hasBlockIO ioops
                pure ()
            -- When IO result have been collected, intra-page lookups searches
            -- through the raw bytes (representing a disk page) for the lookup
            -- key. Again, we use perRunEnv here because IOOps contain mutable
            -- buffers, so we want fresh ones for each run of the benchmark. The
            -- result is a boxed vector of Maybe Entry, but since the
            -- implementation takes care to evaluate each of the elements, we
            -- only compute WHNF.
          , bench "Perform intra-page lookups" $
              perRunEnv ( stToIO (prepLookups blooms indexes kopsFiles ks) >>= \(rkixs, ioops) ->
                          FS.submitIO hasBlockIO ioops >>= \ioress ->
                          pure (rkixs, ioops, ioress)
                        ) $ \ ~(rkixs, ioops, ioress) -> do
                !_ <- intraPageLookups resolveV rs ks rkixs ioops ioress
                pure ()
            -- The whole shebang: lookup preparation, doing the IO, and then
            -- performing intra-page-lookups. Again, we evaluate the result to
            -- WHNF because it is the same result that intraPageLookups produces
            -- (see above).
          , bench "Lookups in IO" $
              whnfAppIO (\ks' -> lookupsIO hasBlockIO resolveV rs blooms indexes kopsFiles ks') ks
          ]
  where
    withEnv = envWithCleanup
                (lookupsInBatchesEnv conf)
                lookupsInBatchesCleanup
    -- TODO: pick a better value resolve function
    resolveV = \v1 _v2 -> v1

{-------------------------------------------------------------------------------
  Environments
-------------------------------------------------------------------------------}

-- | Config options describing a benchmarking scenario
data Config = Config {
    -- | Name for the benchmark scenario described by this config.
    name         :: !String
    -- | Number of key\/operation pairs in the run
  , nentries
    -- | Number of positive lookups
  , npos         :: !Int
    -- | Number of negative lookups
  , nneg         :: !Int
    -- | Optional parameters for the io-uring context
  , ioctxps      :: !(Maybe FS.IOCtxParams)
  }

lookupsInBatchesEnv ::
     Config
  -> IO ( FilePath -- ^ Temporary directory
        , FS.HasFS IO FS.HandleIO
        , FS.HasBlockIO IO FS.HandleIO
        , V.Vector (Run (FS.Handle FS.HandleIO))
        , V.Vector SerialisedKey
        )
lookupsInBatchesEnv Config {..} = do
    sysTmpDir <- getCanonicalTemporaryDirectory
    benchTmpDir <- createTempDirectory sysTmpDir "lookupsInBatchesEnv"
    (storedKeys, lookupKeys) <- lookupsEnv (mkStdGen 17) nentries npos nneg
    let hasFS = FS.ioHasFS (FS.MountPoint benchTmpDir)
    hasBlockIO <- FS.ioHasBlockIO hasFS (fromMaybe FS.defaultIOCtxParams ioctxps)
    let wb = WB.fromMap storedKeys
        fsps = RunFsPaths (FS.mkFsPath []) 0
    r <- Run.fromWriteBuffer hasFS hasBlockIO fsps wb
    let nentriesReal = unNumEntries $ Run.runNumEntries r
    assert (nentriesReal == nentries) $ pure ()
    let npagesReal = Run.sizeInPages r
    assert (npagesReal * 42 <= nentriesReal) $ pure ()
    assert (npagesReal * 43 >= nentriesReal) $ pure ()
    pure ( benchTmpDir
         , hasFS
         , hasBlockIO
         , V.singleton r
         , lookupKeys
         )

lookupsInBatchesCleanup ::
     ( FilePath -- ^ Temporary directory
     , FS.HasFS IO FS.HandleIO
     , FS.HasBlockIO IO FS.HandleIO
     , V.Vector (Run (FS.Handle FS.HandleIO))
     , V.Vector SerialisedKey
     )
  -> IO ()
lookupsInBatchesCleanup (tmpDir, hasFS, hasBlockIO, rs, _) = do
    FS.close hasBlockIO
    forM_ rs $ Run.removeReference hasFS
    removeDirectoryRecursive tmpDir

-- | Generate keys to store and keys to lookup
lookupsEnv ::
     StdGen -- ^ RNG
  -> Int -- ^ Number of stored key\/operation pairs
  -> Int -- ^ Number of positive lookups
  -> Int -- ^ Number of negative lookups
  -> IO ( Map SerialisedKey (Entry SerialisedValue SerialisedBlob)
        , V.Vector (SerialisedKey)
        )
lookupsEnv g nentries npos nneg = do
    let  (g1, g') = R.split g
         (g2, g3) = R.split g'
    let (keys, negLookups) = splitAt nentries
                           $ uniformWithoutReplacement @UTxOKey g1 (nentries + nneg)
        posLookups         = sampleUniformWithReplacement g2 npos keys
    let values = take nentries $ List.unfoldr (Just . randomEntry) g3
        entries = Map.fromList $ zip keys values
    lookups <- generate $ shuffle (negLookups ++ posLookups)

    let entries' = Map.mapKeys serialiseKey
              $ Map.map (bimap serialiseValue serialiseBlob) entries
        lookups' = V.fromList $ fmap serialiseKey lookups
    assert (Map.size entries' == nentries) $ pure ()
    assert (length lookups' == npos + nneg) $ pure ()
    pure (entries', lookups')

-- TODO: tweak distribution
randomEntry :: StdGen -> (Entry UTxOValue UTxOBlob, StdGen)
randomEntry g = frequency [
      (20, \g' -> let (!v, !g'') = uniform g' in (Insert v, g''))
    , (1,  \g' -> let (!v, !g'') = uniform g'
                      (!b, !g''') = genBlob g''
                  in  (InsertWithBlob v b, g'''))
    , (2,  \g' -> let (!v, !g'') = uniform g' in (Mupdate v, g''))
    , (2,  \g' -> (Delete, g'))
    ] g

genBlob :: RandomGen g => g -> (UTxOBlob, g)
genBlob !g =
  let (!len, !g') = uniformR (0, 0x2000) g
      (!bs, !g'') = genByteString len g'
  in (UTxOBlob bs, g'')
