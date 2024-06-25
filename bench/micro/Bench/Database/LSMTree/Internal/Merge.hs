{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Bench.Database.LSMTree.Internal.Merge (benchmarks) where

import           Control.Monad (when, zipWithM)
import           Criterion.Main (Benchmark, bench, bgroup)
import qualified Criterion.Main as Cr
import           Data.Bifunctor (first)
import qualified Data.BloomFilter.Hash as Hash
import           Data.Foldable (traverse_)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Database.LSMTree.Extras.Orphans ()
import qualified Database.LSMTree.Extras.Random as R
import           Database.LSMTree.Extras.UTxO
import           Database.LSMTree.Internal.Entry
import qualified Database.LSMTree.Internal.Merge as Merge
import           Database.LSMTree.Internal.Paths (RunFsPaths (..),
                     pathsForRunFiles, runChecksumsPath)
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
import qualified System.Random as R
import           System.Random (StdGen, mkStdGen, uniform, uniformR)

benchmarks :: Benchmark
benchmarks = bgroup "Bench.Database.LSMTree.Internal.Merge" [
      -- various numbers of runs
      benchMerge configWord64
        { name         = "word64-insert-x2"
        , nentries     = totalEntries `splitInto` 2
        , finserts     = 1
        }
    , benchMerge configWord64
        { name         = "word64-insert-x4"
        , nentries     = totalEntries `splitInto` 4
        , finserts     = 1
        }
    , benchMerge configWord64
        { name         = "word64-insert-x7"
        , nentries     = totalEntries `splitInto` 7
        , finserts     = 1
        }
    , benchMerge configWord64
        { name         = "word64-insert-x13"
        , nentries     = totalEntries `splitInto` 13
        , finserts     = 1
        }
      -- different operations
    , benchMerge configWord64
        { name         = "word64-delete-x4"
        , nentries     = totalEntries `splitInto` 4
        , fdeletes     = 1
        }
    , benchMerge configWord64
        { name         = "word64-blob-x4"
        , nentries     = totalEntries `splitInto` 4
        , fblobinserts = 1
        }
    , benchMerge configWord64
        { name         = "word64-mupsert-x4"  -- basically no collisions
        , nentries     = totalEntries `splitInto` 4
        , fmupserts    = 1
        , mergeMappend = Just (onDeserialisedValues ((+) @Word64))
        }
    , benchMerge configWord64
        { name         = "word64-mupsert-collisions-x4"
        , nentries     = totalEntries `splitInto` 4
        , fmupserts    = 1
        , randomKey    = -- each run uses half of the possible keys
                         randomWord64OutOf (totalEntries `div` 2)
        , mergeMappend = Just (onDeserialisedValues ((+) @Word64))
        }
    , benchMerge configWord64
        { name         = "word64-mix-x4"
        , nentries     = totalEntries `splitInto` 4
        , finserts     = 1
        , fdeletes     = 1
        , fmupserts    = 1
        , mergeMappend = Just (onDeserialisedValues ((+) @Word64))
        }
    , benchMerge configWord64
        { name         = "word64-mix-collisions-x4"
        , nentries     = totalEntries `splitInto` 4
        , finserts     = 1
        , fdeletes     = 1
        , fmupserts    = 1
        , randomKey    = -- each run uses half of the possible keys
                         randomWord64OutOf (totalEntries `div` 2)
        , mergeMappend = Just (onDeserialisedValues ((+) @Word64))
        }
      -- not writing anything at all
    , benchMerge configWord64
        { name         = "word64-delete-x4-lastlevel"
        , nentries     = totalEntries `splitInto` 4
        , fdeletes     = 1
        , mergeLevel   = Merge.LastLevel
        }
      -- different key and value sizes
    , benchMerge configWord64
        { name         = "insert-large-keys-x4"  -- potentially long keys
        , nentries     = (totalEntries `div` 10) `splitInto` 4
        , finserts     = 1
        , randomKey    = first serialiseKey . R.randomByteStringR (6, 4000)
        }
    , benchMerge configWord64
        { name         = "insert-mixed-vals-x4"  -- potentially long values
        , nentries     = (totalEntries `div` 10) `splitInto` 4
        , finserts     = 1
        , randomValue  = first serialiseValue . R.randomByteStringR (0, 4000)
        }
    , benchMerge configWord64
        { name         = "insert-page-x4"  -- 1 page
        , nentries     = (totalEntries `div` 10) `splitInto` 4
        , finserts     = 1
        , randomValue  = first serialiseValue . R.randomByteStringR (4056, 4056)
        }
    , benchMerge configWord64
        { name         = "insert-page-plus-byte-x4"  -- 1 page + 1 byte
        , nentries     = (totalEntries `div` 10) `splitInto` 4
        , finserts     = 1
        , randomValue  = first serialiseValue . R.randomByteStringR (4057, 4057)
        }
    , benchMerge configWord64
        { name         = "insert-huge-vals-x4"  -- 1-5 pages
        , nentries     = (totalEntries `div` 10) `splitInto` 4
        , finserts     = 1
        , randomValue  = first serialiseValue . R.randomByteStringR (10_000, 20_000)
        }
      -- common UTxO scenarios
    , benchMerge configUTxO
        { name         = "utxo-x4"  -- like tiering merge
        , nentries     = totalEntries `splitInto` 4
        , finserts     = 1
        , fdeletes     = 1
        }
    , benchMerge configUTxO
        { name         = "utxo-x4-uneven"
        , nentries     = totalEntries `distributed` [1, 3, 1.5, 2.5]
        , finserts     = 1
        , fdeletes     = 1
        }
    , benchMerge configUTxO
        { name         = "utxo-x4-lastlevel"
        , nentries     = totalEntries `splitInto` 4
        , finserts     = 1
        , fdeletes     = 1
        , mergeLevel   = Merge.LastLevel
        }
    , benchMerge configUTxO
        { name         = "utxo-x4+1-min-skewed-lastlevel"  -- live levelling merge
        , nentries     = totalEntries `distributed` [1, 1, 1, 1, 4]
        , finserts     = 1
        , fdeletes     = 1
        , mergeLevel   = Merge.LastLevel
        }
    , benchMerge configUTxO
        { name         = "utxo-x4+1-max-skewed-lastlevel"  -- live levelling merge
        , nentries     = totalEntries `distributed` [1, 1, 1, 1, 16]
        , finserts     = 1
        , fdeletes     = 1
        , mergeLevel   = Merge.LastLevel
        }
    ]
  where
    totalEntries = 50_000

    splitInto :: Int -> Int -> [Int]
    n `splitInto` k = n `distributed` replicate k 1

    distributed :: Int -> [Double] -> [Int]
    n `distributed` weights =
      let total = sum weights
      in [ round (fromIntegral n * w / total)
         | w <- weights
         ]

benchMerge :: Config -> Benchmark
benchMerge conf@Config{name} =
    withEnv $ \ ~(_dir, hasFS, hbio, runs) ->
      bgroup name [
          bench "merge" $
            -- We'd like to do: `whnfAppIO (runs' -> ...) runs`.
            -- However, we also need per-run cleanup to avoid running out of
            -- disk space. We use `perRunEnvWithCleanup`, which has two issues:
            -- 1. Just as `whnfAppIO` etc., it takes an IO action and returns
            --    `Benchmarkable`, which does not compose. As a workaround, we
            --    thread `runs` through the environment, too.
            -- 2. It forces the result to normal form, which would traverse the
            --    whole run, so we force to WHNF ourselves and just return `()`.
            Cr.perRunEnvWithCleanup
              (pure (runs, outputRunPaths))
              (const (removeOutputRunFiles hasFS)) $ \(runs', p) -> do
                !run <- merge hasFS hbio conf p runs'
                -- Make sure to immediately close resulting runs so we don't run
                -- out of file handles. Ideally this would not be measured, but at
                -- least it's pretty cheap.
                Run.removeReference hasFS run
        ]
  where
    withEnv =
        Cr.envWithCleanup
          (mergeEnv conf)
          mergeEnvCleanup

    -- We need to keep the input runs, but remove the freshly created one.
    removeOutputRunFiles :: FS.HasFS IO FS.HandleIO -> IO ()
    removeOutputRunFiles hasFS = do
        traverse_ (FS.removeFile hasFS) (pathsForRunFiles outputRunPaths)
        exists <- FS.doesFileExist hasFS (runChecksumsPath outputRunPaths)
        when exists $
          FS.removeFile hasFS (runChecksumsPath outputRunPaths)

merge ::
     FS.HasFS IO FS.HandleIO
  -> FS.HasBlockIO IO FS.HandleIO
  -> Config
  -> Run.RunFsPaths
  -> InputRuns
  -> IO (Run (FS.Handle (FS.HandleIO)))
merge fs hbio Config {..} targetPaths runs = do
    let f = fromMaybe const mergeMappend
    m <- fromMaybe (error "empty inputs, no merge created") <$>
      Merge.new fs mergeLevel f targetPaths runs
    go m
  where
    go m =
        Merge.steps fs hbio m stepSize >>= \case
          (_, Merge.MergeComplete run) -> return run
          (_, Merge.MergeInProgress) -> go m

outputRunPaths :: Run.RunFsPaths
outputRunPaths = RunFsPaths (FS.mkFsPath []) 0

inputRunPaths :: [Run.RunFsPaths]
inputRunPaths = RunFsPaths (FS.mkFsPath []) <$> [1..]

type InputRuns = [Run (FS.Handle FS.HandleIO)]

type Mappend = SerialisedValue -> SerialisedValue -> SerialisedValue

onDeserialisedValues :: SerialiseValue v => (v -> v -> v) -> Mappend
onDeserialisedValues f x y =
    serialiseValue (f (deserialiseValue x) (deserialiseValue y))

type SerialisedKOp = (SerialisedKey, SerialisedEntry)
type SerialisedEntry = Entry SerialisedValue SerialisedBlob

{-------------------------------------------------------------------------------
  Environments
-------------------------------------------------------------------------------}

-- | Config options describing a benchmarking scenario
data Config = Config {
    -- | Name for the benchmark scenario described by this config.
    name         :: !String
    -- | Number of key\/operation pairs, one for each run.
  , nentries     :: ![Int]
    -- | Frequency of inserts within the key\/op pairs.
  , finserts     :: !Int
    -- | Frequency of inserts with blobs within the key\/op pairs.
  , fblobinserts :: !Int
    -- | Frequency of deletes within the key\/op pairs.
  , fdeletes     :: !Int
    -- | Frequency of mupserts within the key\/op pairs.
  , fmupserts    :: !Int
  , randomKey    :: Rnd SerialisedKey
  , randomValue  :: Rnd SerialisedValue
  , randomBlob   :: Rnd SerialisedBlob
  , mergeLevel   :: !Merge.Level
    -- | Needs to be defined when generating mupserts.
  , mergeMappend :: !(Maybe Mappend)
    -- | Merging is done in chunks of @stepSize@ entries.
  , stepSize     :: !Int
  }

type Rnd a = StdGen -> (a, StdGen)

defaultConfig :: Config
defaultConfig = Config {
    name         = "default"
  , nentries     = []
  , finserts     = 0
  , fblobinserts = 0
  , fdeletes     = 0
  , fmupserts    = 0
  , randomKey    = error "randomKey not implemented"
  , randomValue  = error "randomValue not implemented"
  , randomBlob   = error "randomBlob not implemented"
  , mergeLevel   = Merge.MidLevel
  , mergeMappend = Nothing
  , stepSize     = maxBound  -- by default, just do in one go
  }

configWord64 :: Config
configWord64 = defaultConfig {
    randomKey    = first serialiseKey . uniform @_ @Word64
  , randomValue  = first serialiseValue . uniform @_ @Word64
  , randomBlob   = first serialiseBlob . R.randomByteStringR (0, 0x2000)  -- up to 8 kB
  }

configUTxO :: Config
configUTxO = defaultConfig {
    randomKey    = first serialiseKey . uniform @_ @UTxOKey
  , randomValue  = first serialiseValue . uniform @_ @UTxOValue
  }

mergeEnv ::
     Config
  -> IO ( FilePath -- ^ Temporary directory
        , FS.HasFS IO FS.HandleIO
        , FS.HasBlockIO IO FS.HandleIO
        , InputRuns
        )
mergeEnv config = do
    sysTmpDir <- getCanonicalTemporaryDirectory
    benchTmpDir <- createTempDirectory sysTmpDir "mergeEnv"
    let hasFS = FS.ioHasFS (FS.MountPoint benchTmpDir)
    hasBlockIO <- FS.ioHasBlockIO hasFS FS.defaultIOCtxParams
    runs <- randomRuns hasFS hasBlockIO config (mkStdGen 17)
    pure (benchTmpDir, hasFS, hasBlockIO, runs)

mergeEnvCleanup ::
     ( FilePath -- ^ Temporary directory
     , FS.HasFS IO FS.HandleIO
     , FS.HasBlockIO IO FS.HandleIO
     , InputRuns
     )
  -> IO ()
mergeEnvCleanup (tmpDir, hasFS, hasBlockIO, runs) = do
    FS.close hasBlockIO
    traverse_ (Run.removeReference hasFS) runs
    removeDirectoryRecursive tmpDir

-- | Generate keys and entries to insert into the write buffer.
-- They are already serialised to exclude the cost from the benchmark.
randomRuns ::
     FS.HasFS IO FS.HandleIO
  -> FS.HasBlockIO IO FS.HandleIO
  -> Config
  -> StdGen
  -> IO InputRuns
randomRuns hasFS hasBlockIO config@Config {..} =
      zipWithM (createRun hasFS hasBlockIO mergeMappend) inputRunPaths
    . zipWith (randomKOps config) nentries
    . List.unfoldr (Just . R.split)

createRun ::
     FS.HasFS IO h
  -> FS.HasBlockIO IO h
  -> Maybe Mappend
  -> Run.RunFsPaths
  -> [SerialisedKOp]
  -> IO (Run (FS.Handle h))
createRun hasFS hasBlockIO mMappend targetPath =
      Run.fromWriteBuffer hasFS hasBlockIO targetPath
    . List.foldl insert WB.empty
  where
    insert wb (k, e) = case mMappend of
      Nothing -> WB.addEntryNormal k (expectNormal e) wb
      Just f  -> WB.addEntryMonoidal f k (expectMonoidal e) wb

    expectNormal e = fromMaybe (error ("invalid normal update: " <> show e))
                       (entryToUpdateNormal e)
    expectMonoidal e = fromMaybe (error ("invalid monoidal update: " <> show e))
                       (entryToUpdateMonoidal e)

-- | Generate keys and entries to insert into the write buffer.
-- They are already serialised to exclude the cost from the benchmark.
randomKOps ::
     Config
  -> Int  -- ^ number of entries
  -> StdGen -- ^ RNG
  -> [SerialisedKOp]
randomKOps Config {..} runentries g0 =
    zip
      (R.withoutReplacement g1 runentries randomKey)
      (R.withReplacement g2 runentries randomEntry)
  where
    (g1, g2) = R.split g0

    randomEntry :: Rnd SerialisedEntry
    randomEntry = R.frequency
        [ ( finserts
          , \g -> let (!v, !g') = randomValue g
                  in  (Insert v, g')
          )
        , ( fblobinserts
          , \g -> let (!v, !g') = randomValue g
                      (!b, !g'') = randomBlob g'
                  in  (InsertWithBlob v b, g'')
          )
        , ( fdeletes
          , \g -> (Delete, g)
          )
        , ( fmupserts
          , \g -> let (!v, !g') = randomValue g
                  in  (Mupdate v, g')
          )
        ]

-- | @randomWord64OutOf n@ generates one out of @n@ distinct keys, which are
-- uniformly distributed.
-- This can be used to make collisions more likely.
--
-- Make sure to pick an @n@ that is significantly larger than the size of a run!
-- Each run entry needs a distinct key.
randomWord64OutOf :: Int -> Rnd SerialisedKey
randomWord64OutOf possibleKeys =
      first (serialiseKey . Hash.hash64)
    . uniformR (0, fromIntegral possibleKeys :: Word64)
