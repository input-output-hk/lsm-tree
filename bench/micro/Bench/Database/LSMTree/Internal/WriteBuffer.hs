{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}

module Bench.Database.LSMTree.Internal.WriteBuffer (benchmarks) where

import           Control.DeepSeq (NFData (..))
import           Criterion.Main (Benchmark, bench, bgroup)
import qualified Criterion.Main as Cr
import           Data.Bifunctor (first)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Database.LSMTree.Extras.Orphans ()
import           Database.LSMTree.Extras.Random (frequency, randomByteStringR)
import           Database.LSMTree.Extras.UTxO
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.Paths (RunFsPaths (..))
import           Database.LSMTree.Internal.Run (Run)
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.Serialise
import           Database.LSMTree.Internal.WriteBuffer (WriteBuffer)
import qualified Database.LSMTree.Internal.WriteBuffer as WB
import qualified Database.LSMTree.Monoidal as Monoidal
import qualified Database.LSMTree.Normal as Normal
import           GHC.Generics
import           Prelude hiding (getContents)
import           System.Directory (removeDirectoryRecursive)
import qualified System.FS.API as FS
import qualified System.FS.BlockIO.API as FS
import qualified System.FS.BlockIO.IO as FS
import qualified System.FS.IO as FS
import           System.IO.Temp
import           System.Random (StdGen, mkStdGen, uniform)

benchmarks :: Benchmark
benchmarks = bgroup "Bench.Database.LSMTree.Internal.WriteBuffer" [
      benchWriteBuffer configWord64
        { name         = "word64-insert-10k"
        , nentries     = 10_000
        , finserts     = 1
        }
    , benchWriteBuffer configWord64
        { name         = "word64-delete-10k"
        , nentries     = 10_000
        , fdeletes     = 1
        }
    , benchWriteBuffer configWord64
        { name         = "word64-blob-10k"
        , nentries     = 10_000
        , fblobinserts = 1
        }
    , benchWriteBuffer configWord64
        { name         = "word64-mupsert-10k"
        , nentries     = 10_000
        , fmupserts    = 1
                         -- TODO: too few collisions to really measure resolution
        , mappendVal   = Just (onDeserialisedValues ((+) @Word64))
        }
      -- different key and value sizes
    , benchWriteBuffer configWord64
        { name         = "insert-large-keys-1k"  -- large keys
        , nentries     = 1_000
        , finserts     = 1
        , randomKey    = first serialiseKey . randomByteStringR (6, 4000)
        }
    , benchWriteBuffer configWord64
        { name         = "insert-mixed-vals-1k"  -- small and large values
        , nentries     = 1_000
        , finserts     = 1
        , randomValue  = first serialiseValue . randomByteStringR (0, 8000)
        }
    , benchWriteBuffer configWord64
        { name         = "insert-page-1k"  -- 1 page
        , nentries     = 1_000
        , finserts     = 1
        , randomValue  = first serialiseValue . randomByteStringR (4056, 4056)
        }
    , benchWriteBuffer configWord64
        { name         = "insert-page-plus-byte-1k"  -- 1 page + 1 byte
        , nentries     = 1_000
        , finserts     = 1
        , randomValue  = first serialiseValue . randomByteStringR (4057, 4057)
        }
    , benchWriteBuffer configWord64
        { name         = "insert-huge-vals-1k"  -- 3-5 pages
        , nentries     = 1_000
        , finserts     = 1
        , randomValue  = first serialiseValue . randomByteStringR (10_000, 20_000)
        }
      -- UTxO workload
      -- compare different buffer sizes to see superlinear cost of map insertion
    , benchWriteBuffer configUTxO
        { name         = "utxo-2k"
        , nentries     = 2_000
        , finserts     = 1
        , fdeletes     = 1
        }
    , benchWriteBuffer configUTxO
        { name         = "utxo-10k"
        , nentries     = 10_000
        , finserts     = 1
        , fdeletes     = 1
        }
    , benchWriteBuffer configUTxO
        { name         = "utxo-50k"
        , nentries     = 50_000
        , finserts     = 1
        , fdeletes     = 1
        }
    ]

benchWriteBuffer :: Config -> Benchmark
benchWriteBuffer conf@Config{name} =
    withEnv $ \ ~(_dir, hasFS, hasBlockIO, kops) ->
      bgroup name [
          bench "insert" $
            Cr.whnf (\kops' -> insert kops') kops
        , Cr.env (pure $ insert kops) $ \wb ->
            bench "flush" $
              Cr.perRunEnvWithCleanup (getPaths hasFS) (const (cleanupPaths hasFS)) $ \p -> do
                !run <- flush hasFS hasBlockIO p wb
                Run.removeReference hasFS run
        , bench "insert+flush" $
            -- To make sure the WriteBuffer really gets recomputed on every run,
            -- we'd like to do: `whnfAppIO (kops' -> ...) kops`.
            -- However, we also need per-run cleanup to avoid running out of
            -- disk space. We use `perRunEnvWithCleanup`, which has two issues:
            -- 1. Just as `whnfAppIO` etc., it takes an IO action and returns
            --    `Benchmarkable`, which does not compose. As a workaround, we
            --    thread `kops` through the environment, too.
            -- 2. It forces the result to normal form, which would traverse the
            --    whole run, so we force to WHNF ourselves and just return `()`.
            Cr.perRunEnvWithCleanup
              ((,) kops <$> getPaths hasFS)
              (const (cleanupPaths hasFS)) $ \(kops', p) -> do
                !run <- flush hasFS hasBlockIO p (insert kops')
                -- Make sure to immediately close runs so we don't run out of
                -- file handles. Ideally this would not be measured, but at
                -- least it's pretty cheap.
                Run.removeReference hasFS run
        ]
  where
    withEnv =
        Cr.envWithCleanup
          (writeBufferEnv conf)
          writeBufferEnvCleanup

    runDir = FS.mkFsPath [name]

    -- We'll remove the files on every run, so we can re-use the same run number.
    getPaths :: FS.HasFS IO FS.HandleIO -> IO RunFsPaths
    getPaths hasFS = do
      FS.createDirectory hasFS runDir
      pure (RunFsPaths runDir 0)

    -- Simply remove the whole active directory.
    cleanupPaths :: FS.HasFS IO FS.HandleIO -> IO ()
    cleanupPaths hasFS = FS.removeDirectoryRecursive hasFS runDir

insert :: InputKOps -> WriteBuffer
insert (NormalInputs kops) =
    List.foldl' (\wb (k, e) -> WB.addEntryNormal k e wb) WB.empty kops
insert (MonoidalInputs kops mappendVal) =
    List.foldl' (\wb (k, e) -> WB.addEntryMonoidal mappendVal k e wb) WB.empty kops

flush :: FS.HasFS IO FS.HandleIO -> FS.HasBlockIO IO FS.HandleIO -> RunFsPaths -> WriteBuffer -> IO (Run (FS.Handle (FS.HandleIO)))
flush = Run.fromWriteBuffer

data InputKOps
  = NormalInputs
      ![(SerialisedKey, Normal.Update SerialisedValue SerialisedBlob)]
  | MonoidalInputs
      ![(SerialisedKey, Monoidal.Update SerialisedValue)]
      !Mappend
  deriving stock Generic
  deriving anyclass NFData

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
    -- | Number of key\/operation pairs in the run
  , nentries     :: !Int
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
    -- | Needs to be defined when generating mupserts.
  , mappendVal   :: !(Maybe Mappend)
  }

type Rnd a = StdGen -> (a, StdGen)

defaultConfig :: Config
defaultConfig = Config {
    name         = "default"
  , nentries     = 0
  , finserts     = 0
  , fblobinserts = 0
  , fdeletes     = 0
  , fmupserts    = 0
  , randomKey    = error "randomKey not implemented"
  , randomValue  = error "randomValue not implemented"
  , randomBlob   = error "randomBlob not implemented"
  , mappendVal   = Nothing
  }

configWord64 :: Config
configWord64 = defaultConfig {
    randomKey    = first serialiseKey . uniform @_ @Word64
  , randomValue  = first serialiseValue . uniform @_ @Word64
  , randomBlob   = first serialiseBlob . randomByteStringR (0, 0x2000)  -- up to 8 kB
  }

configUTxO :: Config
configUTxO = defaultConfig {
    randomKey    = first serialiseKey . uniform @_ @UTxOKey
  , randomValue  = first serialiseValue . uniform @_ @UTxOValue
  }

writeBufferEnv ::
     Config
  -> IO ( FilePath -- ^ Temporary directory
        , FS.HasFS IO FS.HandleIO
        , FS.HasBlockIO IO FS.HandleIO
        , InputKOps
        )
writeBufferEnv config = do
    sysTmpDir <- getCanonicalTemporaryDirectory
    benchTmpDir <- createTempDirectory sysTmpDir "writeBufferEnv"
    let kops = randomKOps config (mkStdGen 17)
    let inputKOps = case mappendVal config of
          Nothing -> NormalInputs (fmap (fmap expectNormal) kops)
          Just f  -> MonoidalInputs (fmap (fmap expectMonoidal) kops) f
    let hasFS = FS.ioHasFS (FS.MountPoint benchTmpDir)
    hasBlockIO <- FS.ioHasBlockIO hasFS FS.defaultIOCtxParams
    pure (benchTmpDir, hasFS, hasBlockIO, inputKOps)
  where
    expectNormal e = fromMaybe (error ("invalid normal update: " <> show e))
                       (entryToUpdateNormal e)
    expectMonoidal e = fromMaybe (error ("invalid monoidal update: " <> show e))
                       (entryToUpdateMonoidal e)

writeBufferEnvCleanup ::
     ( FilePath -- ^ Temporary directory
     , FS.HasFS IO FS.HandleIO
     , FS.HasBlockIO IO FS.HandleIO
     , kops
     )
  -> IO ()
writeBufferEnvCleanup (tmpDir, _, hbio, _) = do
    FS.close hbio
    removeDirectoryRecursive tmpDir

-- | Generate keys and entries to insert into the write buffer.
-- They are already serialised to exclude the cost from the benchmark.
randomKOps ::
     Config
  -> StdGen -- ^ RNG
  -> [SerialisedKOp]
randomKOps Config {..} = take nentries . List.unfoldr (Just . randomKOp)
  where
    randomKOp :: Rnd SerialisedKOp
    randomKOp g = let (!k, !g')  = randomKey g
                      (!e, !g'') = randomEntry g'
                  in  ((k, e), g'')

    randomEntry :: Rnd SerialisedEntry
    randomEntry = frequency
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
