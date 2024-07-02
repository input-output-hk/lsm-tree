{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE NumericUnderscores #-}

module Main ( main ) where

import           Control.Exception (evaluate)
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Bits ((.&.))
import           Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom
import qualified Data.BloomFilter.Hash as Bloom
import qualified Data.BloomFilter.Mutable as MBloom
import           Data.List (foldl')
import           Data.Time
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.WideWord.Word256 (Word256)
import           Data.Word (Word64)
import           GHC.Stats
import           Numeric
import           System.IO
import           System.Mem (performMajorGC)
import           System.Random
import           Text.Printf (printf)

import           Database.LSMTree.Extras.Orphans ()
import           Database.LSMTree.Internal.Monkey as Monkey
import           Database.LSMTree.Internal.Serialise (SerialisedKey,
                     serialiseKey)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  benchmarks

-- Benchmark parameters you can tweak. The defaults are rather small, so the
-- runtime is short and do not show the effects of data sizes no longer fitting
-- into the CPU cache.

-- | The number of entries in the filter in the smallest LSM runs is
-- @2^benchmarkSizeBase@
benchmarkSizeBase :: SizeBase
benchmarkSizeBase = 16

-- | The number of lookups to do. This has to be smaller than the total size of
-- all the filters (otherwise we will not get true positive probes, which is
-- part of the point of this benchmark).
benchmarkNumLookups :: Int
benchmarkNumLookups = 25_000_000

-- A value of 0.02 results in 5 hashes and just over 8 bits per key.
benchmarkRequestedFPR :: RequestedFPR
benchmarkRequestedFPR = 0.02

benchmarks :: IO ()
benchmarks = do
#ifdef NO_IGNORE_ASSERTS
    putStrLn "BENCHMARKING A BUILD WITH -fno-ignore-asserts"
#endif

    enabled <- getRTSStatsEnabled
    when (not enabled) $ fail "Need RTS +T statistics enabled"
    let filterSizes = lsmStyleBloomFilters benchmarkSizeBase
                                          benchmarkRequestedFPR
    putStrLn "Bloom filter stats:"
    putStrLn "(numEntries, sizeFactor, numBits, numHashFuncs)"
    mapM_ print filterSizes
    putStrLn $ "total number of entries:\t " ++ show (totalNumEntries filterSizes)
    putStrLn $ "total filter size in bytes:\t " ++ show (totalNumBytes filterSizes)
    putStrLn $ "total number of key lookups:\t " ++ show benchmarkNumLookups

    unless (totalNumEntriesSanityCheck benchmarkSizeBase filterSizes) $
      fail "totalNumEntriesSanityCheck failed"
    unless (totalNumEntries filterSizes >= benchmarkNumLookups) $
      fail "number of key lookups is more than number of entries"

    putStrLn "Generating bloom filters..."
    let rng0 = mkStdGen 42
    vbs <- elemManyEnv filterSizes rng0
    putStrLn " finished."
    putStrLn ""

    baseline <-
      benchmark "baseline"
                "(This is the cost of just computing the keys.)"
                (benchBaseline vbs rng0) benchmarkNumLookups
                (0, 0)
#ifdef NO_IGNORE_ASSERTS
                80  -- https://gitlab.haskell.org/ghc/ghc/-/issues/24625
#else
                48
#endif

    hashcost <-
      benchmark "makeCheapHashes"
                "(This includes the cost of hashing the keys, less the cost of computing the keys)"
                (benchMakeCheapHashes vbs rng0) benchmarkNumLookups
                baseline
                0

    _ <-
      benchmark "elemCheapHashes"
                "(this is the simple one-by-one lookup, less the cost of computing and hashing the keys)"
                (benchElemCheapHashes vbs rng0) benchmarkNumLookups
                (fst hashcost + fst baseline, snd hashcost + snd baseline)
                0

    return ()

type Alloc = Int

benchmark :: String
          -> String
          -> (Int -> ())
          -> Int
          -> (NominalDiffTime, Alloc)
          -> Int
          -> IO (NominalDiffTime, Alloc)
benchmark name description action n (subtractTime, subtractAlloc) expectedAlloc = do
    putStrLn $ "Benchmarking " ++ name ++ " ... "
    putStrLn description
    performMajorGC
    allocBefore <- allocated_bytes <$> getRTSStats
    timeBefore  <- getCurrentTime
    evaluate (action n)
    timeAfter   <- getCurrentTime
    performMajorGC
    allocAfter  <- allocated_bytes <$> getRTSStats
    putStrLn "Finished."
    let allocTotal :: Alloc
        timeTotal  = timeAfter `diffUTCTime` timeBefore
        allocTotal = fromIntegral allocAfter - fromIntegral allocBefore
        timeNet    = timeTotal - subtractTime
        allocNet   = allocTotal - subtractAlloc

        timePerKey, allocPerKey :: Double
        timePerKey     = realToFrac timeNet / fromIntegral n
        allocPerKey    = fromIntegral allocNet / fromIntegral n
    let printStat :: String -> Double -> String -> IO ()
        printStat label v unit =
          putStrLn $ label ++ showGFloat (Just 2) v (' ':unit)
    printStat "Time total:        " (realToFrac timeTotal) "seconds"
    printStat "Alloc total:       " (fromIntegral allocTotal) "bytes"
    printStat "Time net:          " (realToFrac timeNet) "seconds"
    printStat "Alloc net:         " (fromIntegral allocNet) "bytes"
    printStat "Time net per key:  " timePerKey "seconds"
    printStat "Alloc net per key: " allocPerKey "bytes"

    unless (truncate allocPerKey == expectedAlloc) $ do
        printf "WARNING: expecting %d, got %d bytes allocated per key\n"
            expectedAlloc
            (truncate allocPerKey :: Int)

    putStrLn ""
    return (timeNet, allocNet)

-- | (numEntries, sizeFactor, numBits, numHashFuncs)
type BloomFilterSizeInfo = (Int, Int, Word64, Int)
type SizeBase     = Int
type RequestedFPR = Double

-- | Calculate the sizes of a realistic LSM style set of Bloom filters, one
-- for each LSM run. This uses base 4, with 4 disk levels, using tiering
-- for internal levels and leveling for the final (biggest) level.
--
-- Due to the incremental merging, each level actually has (in the worst case)
-- 2x the number of runs, hence 8 per level for tiering levels.
--
lsmStyleBloomFilters :: SizeBase -> RequestedFPR -> [BloomFilterSizeInfo]
lsmStyleBloomFilters l1 requestedFPR =
    [ (numEntries, sizeFactor, fromIntegral numBits, numHashFuncs)
    | (numEntries, sizeFactor)
        <- replicate 8 (2^(l1+0), 1)   -- 8 runs at level 1 (tiering)
        ++ replicate 8 (2^(l1+2), 4)   -- 8 runs at level 2 (tiering)
        ++ replicate 8 (2^(l1+4),16)   -- 8 runs at level 3 (tiering)
        ++            [(2^(l1+8),256)] -- 1 run  at level 4 (leveling)
    , let numBits      = Monkey.monkeyBits numEntries requestedFPR
          numHashFuncs = Monkey.monkeyHashFuncs numBits numEntries
    ]

totalNumEntries, totalNumBytes :: [BloomFilterSizeInfo] -> Int
totalNumEntries filterSizes =
    sum [ numEntries | (numEntries, _, _, _) <- filterSizes ]

totalNumBytes filterSizes =
    fromIntegral $ sum [ numBits | (_,_,numBits,_) <- filterSizes ] `div` 8

totalNumEntriesSanityCheck :: SizeBase -> [BloomFilterSizeInfo] -> Bool
totalNumEntriesSanityCheck l1 filterSizes =
    totalNumEntries filterSizes
    ==
    sum [ 2^l1 * sizeFactor | (_, sizeFactor, _, _) <- filterSizes ]


-- | Input environment for benchmarking 'Bloom.elemMany'.
--
-- The idea here is to have a collection of bloom filters corresponding to
-- the sizes used in a largeish LSM. In particular, the sizes are in increasing
-- powers of 4, and the largest ones should be bigger than the CPU L3 cache.
-- Furthermore, the keys in the filters are non-overlapping, and lookups will
-- be true positives in only one filter. Thus most lookups will be true
-- negatives.
--
-- The goal is to benchmark the benefits of optimisations for the LSM situation:
--
-- * where the same key is being looked up in many filters,
-- * with a hit in only one filter expected, and
-- * where the total size of the filters is too large to fully fit in cache
--   (though the smaller ones may fit in the caches).
--
elemManyEnv :: [BloomFilterSizeInfo]
            -> StdGen
            -> IO (Vector (Bloom SerialisedKey))
elemManyEnv filterSizes rng0 =
  stToIO $ do
    -- create the filters
    mbs <- sequence
             [ MBloom.new numHashFuncs numBits
             | (_, _, numBits, numHashFuncs) <- filterSizes ]
    -- add elements
    foldM_
      (\rng (i, mb) -> do
         -- progress
         when (i .&. 0xFFFF == 0) (unsafeIOToST $ putStr ".")
         -- insert n elements into filter b
         let k :: Word256
             (!k, !rng') = uniform rng
         MBloom.insert mb (serialiseKey k)
         return rng'
      )
      rng0
      (zip [0 .. totalNumEntries filterSizes - 1]
           (cycle [ mb'
                  | (mb, (_, sizeFactor, _, _)) <- zip mbs filterSizes
                  , mb' <- replicate sizeFactor mb ]))
    Vector.fromList <$> mapM Bloom.unsafeFreeze mbs

-- | This gives us a baseline cost of just calculating the series of keys.
benchBaseline :: Vector (Bloom SerialisedKey) -> StdGen -> Int -> ()
benchBaseline !_  !_   0 = ()
benchBaseline !bs !rng !n =
    let k :: Word256
        (!k, !rng') = uniform rng
        !k' = serialiseKey k
     in k' `seq` benchBaseline bs rng' (n-1)

-- | This gives us a combined cost of calculating the series of keys and their
-- hashes.
benchMakeCheapHashes :: Vector (Bloom SerialisedKey) -> StdGen -> Int -> ()
benchMakeCheapHashes !_  !_   0 = ()
benchMakeCheapHashes !bs !rng !n =
    let k :: Word256
        (!k, !rng') = uniform rng
        !kh = Bloom.makeHashes (serialiseKey k) :: Bloom.CheapHashes SerialisedKey
     in kh `seq` benchMakeCheapHashes bs rng' (n-1)

-- | This gives us a combined cost of calculating the series of keys, their
-- hashes, and then using 'Bloom.elemCheapHashes' with each filter.
benchElemCheapHashes :: Vector (Bloom SerialisedKey) -> StdGen -> Int -> ()
benchElemCheapHashes !_  !_   0  = ()
benchElemCheapHashes !bs !rng !n =
    let k :: Word256
        (!k, !rng') = uniform rng
        !kh =  Bloom.makeHashes (serialiseKey k)
     in foldl' (\_ b -> Bloom.elemHashes kh b `seq` ()) () bs
  `seq` benchElemCheapHashes bs rng' (n-1)

