{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Database.LSMTree.Internal.Lookup (
    tests
    -- * internals
  , InMemLookupData (..)
  , opaqueifyBlobs
  , SmallList (..)
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.ST.Strict
import           Data.Bifunctor
import           Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom
import           Data.Coerce (coerce)
import           Data.Either (rights)
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid (Endo (..))
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import           Data.Word
import           Database.LSMTree.Extras
import           Database.LSMTree.Extras.Generators
import           Database.LSMTree.Internal.BlobRef (BlobSpan)
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.IndexCompact as Index
import           Database.LSMTree.Internal.Lookup
import           Database.LSMTree.Internal.Paths (RunFsPaths (..))
import qualified Database.LSMTree.Internal.RawBytes as RB
import           Database.LSMTree.Internal.RawOverflowPage
import           Database.LSMTree.Internal.RawPage
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.RunAcc as Run
import           Database.LSMTree.Internal.Serialise
import           Database.LSMTree.Internal.Serialise.Class
import qualified Database.LSMTree.Internal.WriteBuffer as WB
import           GHC.Generics
import qualified System.FS.API as FS
import           System.FS.API (Handle (..), mkFsPath)
import qualified System.FS.BlockIO.API as FS
import           System.FS.BlockIO.API
import           Test.Database.LSMTree.Generators (deepseqInvariant,
                     prop_arbitraryAndShrinkPreserveInvariant,
                     prop_forAllArbitraryAndShrinkPreserveInvariant)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.FS (withTempIOHasBlockIO)
import           Test.Util.QuickCheck as Util.QC

tests :: TestTree
tests = testGroup "Test.Database.LSMTree.Internal.Lookup" [
      testGroup "models" [
          testProperty "prop_bloomQueriesModel" $
            forAllShrink arbitrary shrink prop_bloomQueriesModel
        , testProperty "prop_indexSearchesModel" $
            forAllShrink arbitrary shrink prop_indexSearchesModel
        , testProperty "prop_prepLookupsModel" $
            forAllShrink arbitrary shrink prop_prepLookupsModel
        , testProperty "input distribution" $ \dats ->
            tabulateInMemLookupDataN (getSmallList dats) True
        ]
    , testGroup "With multi-page values" [
          testGroup "InMemLookupData" $
            prop_arbitraryAndShrinkPreserveInvariant (deepseqInvariant @(InMemLookupData SerialisedKey SerialisedValue BlobSpan))
        , localOption (QuickCheckMaxSize 1000) $
          testProperty "prop_inMemRunLookupAndConstruction" prop_inMemRunLookupAndConstruction
        ]
    , testGroup "Without multi-page values" [
          testGroup "InMemLookupData" $
            prop_forAllArbitraryAndShrinkPreserveInvariant
              genNoMultiPage
              shrinkNoMultiPage
              (deepseqInvariant @(InMemLookupData SerialisedKey SerialisedValue BlobSpan))
        , localOption (QuickCheckMaxSize 1000) $
          testProperty "prop_inMemRunLookupAndConstruction" $
            forAllShrink genNoMultiPage shrinkNoMultiPage prop_inMemRunLookupAndConstruction
        ]

    , testProperty "Roundtrip from write buffer then batched lookups" $
        prop_roundtripFromWriteBufferLookupIO
    ]
  where
    genNoMultiPage = liftArbitrary2 arbitrary arbitrary
    shrinkNoMultiPage = liftShrink2 shrink shrink

{-------------------------------------------------------------------------------
  Models
-------------------------------------------------------------------------------}

prop_bloomQueriesModel ::
     SmallList (InMemLookupData SerialisedKey SerialisedValue BlobSpan)
  -> Property
prop_bloomQueriesModel dats =
    realDefault === model .&&. realNonDefault === model
  where
    runs = getSmallList $ fmap (mkTestRun . runData) dats
    blooms = fmap snd3 runs
    lookupss = concatMap lookups $ getSmallList dats
    realDefault = bloomQueriesDefault (V.fromList blooms) (V.fromList lookupss)
    realNonDefault = bloomQueries (V.fromList blooms) (V.fromList lookupss) 10
    model = VU.fromList $ bloomQueriesModel blooms lookupss

bloomQueriesModel :: [Bloom SerialisedKey] -> [SerialisedKey] -> [(RunIx, KeyIx)]
bloomQueriesModel blooms ks = [
      (rix, kix)
    | (rix, b) <- rs'
    , (kix, k) <- ks'
    , Bloom.elem k b
    ]
  where
    rs' = zip [0..] blooms
    ks' = zip [0..] ks

prop_indexSearchesModel ::
     SmallList (InMemLookupData SerialisedKey SerialisedValue BlobSpan)
  -> Property
prop_indexSearchesModel dats =
    forAllShrink (rkixsGen rkixsAll) shrink $ \rkixs ->
           real (VU.fromList rkixs) === model rkixs
      .&&. real (VU.fromList rkixs) === model rkixs
      .&&. length rkixs === length (model rkixs)
  where
    rkixsAll = [(rix, kix) | (rix,_) <- zip [0..] runs, (kix,_) <- zip [0..] lookupss]
    rkixsGen [] = pure []
    rkixsGen xs = listOf (elements xs)

    runs = getSmallList $ fmap (mkTestRun . runData) dats
    lookupss = concatMap lookups $ getSmallList dats
    real rkixs = runST $ do
      let rs = V.fromList (fmap runWithHandle runs)
          ks = V.fromList lookupss
      res <- indexSearches (V.map thrd3 rs) (V.map fst3 rs) ks rkixs
      pure $ V.map ioopPageSpan res
    model rkixs = V.fromList $ indexSearchesModel (fmap thrd3 runs) lookupss $ rkixs

indexSearchesModel ::
     [IndexCompact]
  -> [SerialisedKey]
  -> [(RunIx, KeyIx)]
  -> [PageSpan]
indexSearchesModel cs ks rkixs =
    flip fmap rkixs $ \(rix, kix) ->
      let c = cs List.!! rix
          k = ks List.!! kix
      in  Index.search k c

prop_prepLookupsModel ::
     SmallList (InMemLookupData SerialisedKey SerialisedValue BlobSpan)
  -> Property
prop_prepLookupsModel dats = real === model
  where
    runs = getSmallList $ fmap (mkTestRun . runData) dats
    lookupss = concatMap lookups $ getSmallList dats
    real = runST $ do
      let rs = V.fromList (fmap runWithHandle runs)
          ks = V.fromList lookupss
      (kixs, ioops) <- prepLookups
                         (V.map snd3 rs)
                         (V.map thrd3 rs)
                         (V.map fst3 rs) ks
      pure $ (kixs, V.map ioopPageSpan ioops)
    model = bimap VU.fromList V.fromList $
            prepLookupsModel (fmap (\x -> (snd3 x, thrd3 x)) runs) lookupss

prepLookupsModel ::
     [(Bloom SerialisedKey, IndexCompact)]
  -> [SerialisedKey]
  -> ([(RunIx, KeyIx)], [PageSpan])
prepLookupsModel rs ks = unzip
    [ ((rix, kix), pspan)
    | (rix, (b, c)) <- zip [0..] rs
    , (kix, k) <- zip [0..] ks
    , Bloom.elem k b
    , let pspan = Index.search k c
    ]

{-------------------------------------------------------------------------------
  Round-trip
-------------------------------------------------------------------------------}

-- | Construct a run incrementally, then test a number of positive and negative lookups.
prop_inMemRunLookupAndConstruction ::
     InMemLookupData SerialisedKey SerialisedValue BlobSpan
  -> Property
prop_inMemRunLookupAndConstruction dat =
      tabulateInMemLookupData dat run
    $ conjoinF (fmap checkMaybeInRun keysMaybeInRun) .&&. conjoinF (fmap checkNotInRun keysNotInRun)
  where
    InMemLookupData{runData, lookups} = dat

    run = mkTestRun runData
    keys = V.fromList lookups
    -- prepLookups says that a key /could be/ in the given page
    keysMaybeInRun = runST $ do
      (kixs, ioops) <- let r = V.singleton (runWithHandle run)
                       in  prepLookups
                             (V.map snd3 r)
                             (V.map thrd3 r)
                             (V.map fst3 r)
                             keys
      let ks = V.map (V.fromList lookups V.!) (V.convert $ snd $ VU.unzip kixs)
          pss = V.map (handleRaw . ioopHandle) ioops
          pspans = V.map (ioopPageSpan) ioops
      pure $ zip3 (V.toList ks) (V.toList pss) (V.toList pspans)
    -- prepLookups says that a key /is definitely not/ in the given page
    keysNotInRun = Set.toList (Set.fromList lookups Set.\\ Set.fromList (fmap (\(x,_,_) -> x) keysMaybeInRun))

    -- Check that a key /is definitely not/ in the given page.
    checkNotInRun :: SerialisedKey -> Property
    checkNotInRun k =
          tabulate1Pre (classifyBin (isJust truth) False)
        $ counterexample ("checkNotInRun: " <> show k)
        $ truth === test
      where truth = Map.lookup k runData
            test  = Nothing

    -- | Check that a key /could be/ in the given page
    checkMaybeInRun :: ( SerialisedKey
                       , Map Int (Either RawPage RawOverflowPage)
                       , PageSpan )
                    -> Property
    checkMaybeInRun (k, ps, PageSpan (PageNo i) (PageNo j))
      | i <= j    = tabulate "PageSpan size" [showPowersOf10 $ j - i + 1]
                  $ tabulate1Pre (classifyBin (isJust truth) True)
                  $ counterexample ("checkMaybeInRun: " <> show k)
                  $ truth === test
      | otherwise = error "impossible: end of a page span can not come before its start"
      where
        truth = Map.lookup k runData
        test  =
          case ps Map.! i of
            Left rawPage ->
              case rawPageLookup rawPage (coerce k) of
                LookupEntryNotPresent       -> Nothing
                LookupEntry entry           -> Just entry
                LookupEntryOverflow entry n -> Just (first (concatOverflow n) entry)
            Right _rawOverflowPage ->
              error "looked up overflow page!"

        -- read remaining bytes for a multi-page value, and append it to the
        -- prefix we already have
        concatOverflow :: Word32 -> SerialisedValue -> SerialisedValue
        concatOverflow = coerce $ \(n :: Word32) (v :: RawBytes) ->
            v <> RB.take (fromIntegral n)
                         (mconcat $ map rawOverflowPageRawBytes overflowPages)
          where
            start = i + 1
            size  = j - i
            overflowPages :: [RawOverflowPage]
            overflowPages = rights
                          . Map.elems
                          . Map.take size
                          . Map.drop start $ ps

    tabulate1Pre :: BinaryClassification -> Property -> Property
    tabulate1Pre  x = tabulate "Lookup classification: pre intra-page lookup"  [show x]


{-------------------------------------------------------------------------------
  Round-trip lookups in IO
-------------------------------------------------------------------------------}

prop_roundtripFromWriteBufferLookupIO ::
     SmallList (InMemLookupData SerialisedKey SerialisedValue SerialisedBlob)
  -> Property
prop_roundtripFromWriteBufferLookupIO dats =
    ioProperty $ withTempIOHasBlockIO "prop_roundtripFromWriteBufferLookupIO" $ \hasFS hasBlockIO -> do
    (runs, wbs) <- mkRuns hasFS hasBlockIO
    let wbAll = WB.fromMap $ Map.unionsWith (combine resolveV) (fmap WB.toMap wbs)
    real <- lookupsIO
              hasBlockIO
              resolveV
              runs
              (V.map Run.runFilter runs)
              (V.map Run.runIndex runs)
              (V.map Run.runKOpsFile runs)
              lookupss
    let model = WB.lookups wbAll lookupss
    V.mapM_ (Run.removeReference hasFS) runs
    FS.close hasBlockIO
    -- TODO: we don't compare blobs, because we haven't implemented blob
    -- retrieval yet.
    pure $ opaqueifyBlobs model === opaqueifyBlobs real
  where
    mkRuns hasFS hasBlockIO = first V.fromList . unzip <$> sequence [
          (,wb) <$> Run.fromWriteBuffer hasFS hasBlockIO (RunFsPaths (FS.mkFsPath []) i) wb
        | (i, dat) <- zip [0..] (getSmallList dats)
        , let wb = WB.fromMap (runData dat)
        ]
    lookupss = V.fromList $ concatMap lookups dats
    resolveV = \(SerialisedValue v1) (SerialisedValue v2) -> SerialisedValue (v1 <> v2)

opaqueifyBlobs :: V.Vector (Maybe (Entry v b)) -> V.Vector (Maybe (Entry v Opaque))
opaqueifyBlobs = fmap (fmap (fmap Opaque))

{-------------------------------------------------------------------------------
  Utils
-------------------------------------------------------------------------------}

newtype SmallList a = SmallList { getSmallList :: [a] }
  deriving stock (Show, Eq)
  deriving newtype (Functor, Foldable)

instance Arbitrary a => Arbitrary (SmallList a) where
  arbitrary = do
      n <- chooseInt (0, 5)
      SmallList <$> vectorOf n arbitrary
  shrink = fmap SmallList . shrink . getSmallList

conjoinF :: (Testable prop, Foldable f) => f prop -> Property
conjoinF = conjoin . F.toList

ioopPageSpan :: IOOp s h -> PageSpan
ioopPageSpan ioop =
    assert (fst x `mod` 4096 == 0) $
    assert (snd x `mod` 4096 == 0) $
    assert (start >= 0) $
    assert (end >= start)
    PageSpan {
        pageSpanStart = PageNo start
      , pageSpanEnd   = PageNo end
      }
  where
    start = fst x `div` 4096
    size  = (snd x `div` 4096) - 1
    end = start + size

    x = case ioop of
      IOOpRead  _ foff _ _ c -> (fromIntegral foff, fromIntegral c)
      IOOpWrite _ foff _ _ c -> (fromIntegral foff, fromIntegral c)

fst3 :: (a, b, c) -> a
fst3 (h, _, _) = h

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thrd3 :: (a, b, c) -> c
thrd3 (_, _, c) = c

-- | An opaque data type with a trivial 'Eq' instance
data Opaque = forall a. Opaque a

instance Show Opaque where
  show _ = "Opaque"

instance Eq Opaque where
  _ == _ = True

{-------------------------------------------------------------------------------
  Test run
-------------------------------------------------------------------------------}

runWithHandle ::
     TestRun
  -> ( Handle (Map Int (Either RawPage RawOverflowPage))
     , Bloom SerialisedKey, IndexCompact
     )
runWithHandle (rawPages, b, ic) = (Handle rawPages (mkFsPath ["do not use"]), b, ic)

type TestRun = (Map Int (Either RawPage RawOverflowPage), Bloom SerialisedKey, IndexCompact)

mkTestRun :: Map SerialisedKey (Entry SerialisedValue BlobSpan) -> TestRun
mkTestRun dat = (rawPages, b, ic)
  where
    nentries = NumEntries (Map.size dat)
    -- suggested range-finder precision is going to be @0@ anyway unless the
    -- input data is very big
    npages   = 0

    -- one-shot run construction
    (pages, b, ic) = runST $ do
      racc <- Run.new nentries npages Nothing
      let kops = Map.toList dat
      psopss <- traverse (uncurry (Run.addKeyOp racc)) kops
      (mp, _ , b', ic', _) <- Run.unsafeFinalise racc
      let pages' = [ p | (ps, ops, _) <- psopss
                      , p <- map Left ps ++ map Right ops ]
               ++ [ Left p | p <- maybeToList mp ]
      pure (pages', b', ic')

    -- create a mapping of page numbers to raw pages, which we can use to do
    -- intra-page lookups on after first probing the bloom filter and index
    rawPages :: Map Int (Either RawPage RawOverflowPage)
    rawPages   = Map.fromList (zip [0..] pages)

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

-- | Binary classification of truth vs. a test result
classifyBin :: Bool -> Bool -> BinaryClassification
classifyBin truth test
    |     truth &&     test = TruePositive
    | not truth &&     test = FalsePositive
    |     truth && not test = FalseNegative
  --  not truth && not test =
    | otherwise             = TrueNegative

data BinaryClassification =
    TruePositive  | FalsePositive
  | FalseNegative | TrueNegative
  deriving Show

tabulateInMemLookupDataN ::
     forall prop. Testable prop
  => [InMemLookupData SerialisedKey SerialisedValue BlobSpan]
  -> (prop -> Property)
tabulateInMemLookupDataN dats = appEndo (foldMap Endo [
      let run = mkTestRun (runData dat)
      in  tabulateInMemLookupData dat run
    | dat <- dats
    ])
    . tabulate "Number of runs" [show $ length dats]

tabulateInMemLookupData ::
     forall prop. Testable prop
  => InMemLookupData SerialisedKey SerialisedValue BlobSpan
  -> TestRun
  -> (prop -> Property)
tabulateInMemLookupData dat run =
        tabulateKeySizes
      . tabulateValueSizes
      . tabulateNumKeyEntryPairs
      . tabulateNumPages
      . tabulateNumLookups
  where
    InMemLookupData{runData, lookups} = dat
    tabulateKeySizes = tabulate "Size of key in run" [showPowersOf10 $ sizeofKey k | k <- Map.keys runData ]
    tabulateValueSizes = tabulate "Size of value in run" [showPowersOf10 $ onValue 0 sizeofValue e | e <- Map.elems runData]
    tabulateNumKeyEntryPairs = tabulate "Number of key-entry pairs" [showPowersOf10 (Map.size runData) ]
    tabulateNumPages = tabulate "Number of pages" [showPowersOf10 (Map.size ps) | let (ps,_,_) = run]
    tabulateNumLookups = tabulate "Number of lookups" [showPowersOf10 (length lookups)]

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

data InMemLookupData k v b = InMemLookupData {
    -- | Data for constructing a run
    runData :: Map k (Entry v b)
    -- | Keys to look up. Expected lookup results are obtained by querying
    -- runData.
  , lookups :: [k]
  }
  deriving stock (Show, Generic)
  deriving anyclass NFData

instance Arbitrary (InMemLookupData SerialisedKey SerialisedValue BlobSpan) where
  arbitrary = liftArbitrary3InMemLookupData genSerialisedKey genSerialisedValue genBlobSpan
  shrink = liftShrink3InMemLookupData shrinkSerialisedKey shrinkSerialisedValue shrinkBlobSpan

instance Arbitrary1 (InMemLookupData SerialisedKey SerialisedValue) where
  liftArbitrary = liftArbitrary3InMemLookupData genSerialisedKey genSerialisedValue

instance Arbitrary2 (InMemLookupData SerialisedKey) where
  liftArbitrary2 = liftArbitrary3InMemLookupData genSerialisedKey

liftArbitrary3InMemLookupData ::
     Ord k
  => Gen k
  -> Gen v
  -> Gen b
  -> Gen (InMemLookupData k v b)
liftArbitrary3InMemLookupData genKey genValue genBlob = do
    kops <- liftArbitrary2Map genKey (liftArbitrary genEntry)
              `suchThat` (\x -> Map.size (Map.filter isJust x) > 0)
    let runData = Map.mapMaybe id kops
    lookups <- (sublistOf (Map.keys kops) >>= shuffle)
    pure InMemLookupData{ runData, lookups }
  where
    genEntry = liftArbitrary2 genValue genBlob

liftShrink3InMemLookupData ::
     Ord k
  => (k -> [k])
  -> (v -> [v])
  -> (b -> [b])
  -> InMemLookupData k v b
  -> [InMemLookupData k v b]
liftShrink3InMemLookupData shrinkKey shrinkValue shrinkBlob InMemLookupData{ runData, lookups } =
         [ InMemLookupData runData' lookups
         | runData' <- liftShrink2Map shrinkKey shrinkEntry runData
         , Map.size runData' > 0 ]
      ++ [ InMemLookupData runData lookups'
         | lookups' <- liftShrink shrinkKey lookups ]
    where
      shrinkEntry = liftShrink2 shrinkValue shrinkBlob

genSerialisedKey :: Gen SerialisedKey
genSerialisedKey = frequency [
      (9, arbitrary `suchThat` (\k -> sizeofKey k >= 6))
    , (1, do x <- getSmall <$> arbitrary
             pure $ SerialisedKey (RB.pack [0,0,0,0,0,0, x]))
    ]

shrinkSerialisedKey :: SerialisedKey -> [SerialisedKey]
shrinkSerialisedKey k = [k' | k' <- shrink k, sizeofKey k' >= 6]

genSerialisedValue :: Gen SerialisedValue
genSerialisedValue = frequency [ (50, arbitrary), (1, genLongValue) ]
  where genLongValue = coerce ((<>) <$> genRawBytesSized 65536 <*> arbitrary)

-- Shrinking as lists can normally be quite slow, so if the value is larger than
-- a threshold, use less exhaustive shrinking.
shrinkSerialisedValue :: SerialisedValue -> [SerialisedValue]
shrinkSerialisedValue v
  | sizeofValue v > 64 = -- shrink towards fewer bytes
                          [ coerce RB.take n' v | n' <- shrinkIntegral n ]
                          -- shrink towards a value of all 0-bytes
                      ++ [ v' | let v' = coerce (VP.fromList $ replicate n 0), v' /= v ]
  | otherwise          = shrink v -- expensive, but thorough
  where n = sizeofValue v

genBlobSpan :: Gen BlobSpan
genBlobSpan = arbitrary

shrinkBlobSpan :: BlobSpan -> [BlobSpan]
shrinkBlobSpan = shrink

instance Arbitrary (InMemLookupData SerialisedKey SerialisedValue SerialisedBlob) where
  arbitrary = liftArbitrary3InMemLookupData genSerialisedKey genSerialisedValue genSerialisedBlob
  shrink = liftShrink3InMemLookupData shrinkSerialisedKey shrinkSerialisedValue shrinkSerialisedBlob

genSerialisedBlob :: Gen SerialisedBlob
genSerialisedBlob = arbitrary

shrinkSerialisedBlob :: SerialisedBlob -> [SerialisedBlob]
shrinkSerialisedBlob = shrink
