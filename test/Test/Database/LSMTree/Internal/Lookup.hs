{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Database.LSMTree.Internal.Lookup (tests) where

import           Control.DeepSeq
import           Control.Monad.ST.Strict
import           Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as SBS
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Primitive.ByteArray (ByteArray (ByteArray),
                     sizeofByteArray)
import qualified Data.Set as Set
import qualified Data.Vector.Primitive as P
import           Data.Word
import           Database.LSMTree.Generators
import           Database.LSMTree.Internal.BlobRef (BlobSpan)
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.Lookup
import           Database.LSMTree.Internal.RawPage
import           Database.LSMTree.Internal.Run.BloomFilter as Bloom
import           Database.LSMTree.Internal.Run.Construction as Run
import           Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise
import qualified Database.LSMTree.Internal.Serialise.RawBytes as RB
import           Database.LSMTree.Util
import           GHC.Generics
import           Test.Database.LSMTree.Generators (deepseqInvariant,
                     prop_arbitraryAndShrinkPreserveInvariant,
                     prop_forAllArbitraryAndShrinkPreserveInvariant)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans ()
import           Test.Util.QuickCheck as Util.QC

tests :: TestTree
tests = testGroup "Test.Database.LSMTree.Internal.Integration" [
      testGroup "With multi-page values" [
          testGroup "InMemLookupData" $
            prop_arbitraryAndShrinkPreserveInvariant (deepseqInvariant @(InMemLookupData SerialisedKey SerialisedValue))
        , localOption (QuickCheckMaxSize 1000) $
          testProperty "prop_inMemRunLookupAndConstruction" prop_inMemRunLookupAndConstruction

        ]
    , testGroup "Without multi-page values" [
          testGroup "InMemLookupData" $
            prop_forAllArbitraryAndShrinkPreserveInvariant
              genNoMultiPage
              shrinkNoMultiPage
              (deepseqInvariant @(InMemLookupData SerialisedKey SerialisedValue))
        , localOption (QuickCheckMaxSize 1000) $
          testProperty "prop_inMemRunLookupAndConstruction" $
            forAllShrink genNoMultiPage shrinkNoMultiPage prop_inMemRunLookupAndConstruction
        ]
    ]
  where
    genNoMultiPage = liftArbitrary arbitrary
    shrinkNoMultiPage = liftShrink shrink

-- | Construct a run incrementally, then test a number of positive and negative lookups.
prop_inMemRunLookupAndConstruction :: InMemLookupData SerialisedKey SerialisedValue -> Property
prop_inMemRunLookupAndConstruction dat =
    Map.size runData > 0 ==>
        tabulateKeySizes
      $ tabulateValueSizes
      $ tabulateNumKeyEntryPairs
      $ tabulateNumPages
      $ tabulateNumLookups
      $ conjoin (fmap checkMaybeInRun keysMaybeInRun) .&&. conjoin (fmap checkNotInRun keysNotInRun)
  where
    InMemLookupData{runData, lookups} = dat

    tabulateKeySizes = tabulate "Size of key in run" [showPowersOf10 $ sizeofKey k | k <- Map.keys runData ]
    tabulateValueSizes = tabulate "Size of value in run" [showPowersOf10 $ onValue 0 sizeofValue e | e <- Map.elems runData]
    tabulateNumKeyEntryPairs = tabulate "Number of key-entry pairs" [showPowersOf10 (Map.size runData) ]
    tabulateNumPages = tabulate "Number of pages" [showPowersOf10 (Map.size ps) | let (ps, _, _) = run]
    tabulateNumLookups = tabulate "Number of lookups" [showPowersOf10 (length lookups)]

    run = mkTestRun runData
    -- prepLookups says that a key /could be/ in the given page
    keysMaybeInRun = prepLookups [run] lookups
    -- prepLookups says that a key /is definitely not/ in the given page
    keysNotInRun = Set.toList (Set.fromList lookups Set.\\ Set.fromList (fmap fst keysMaybeInRun))

    -- Check that a key /is definitely not/ in the given page.
    checkNotInRun :: SerialisedKey -> Property
    checkNotInRun k =
          tabulate1Pre (classifyBin (isJust truth) False)
        $ truth === test
      where truth = Map.lookup k runData
            test  = Nothing

    -- | Check that a key /could be/ in the given page
    checkMaybeInRun :: (SerialisedKey, (Map Int RawPage, PageSpan)) -> Property
    checkMaybeInRun (k, (ps, PageSpan (PageNo i) (PageNo j)))
      | i <= j    = tabulate "PageSpan size" [showPowersOf10 $ j - i + 1]
                  $ tabulate1Pre (classifyBin (isJust truth) True)
                  $ truth === test
      | otherwise = error "impossible: end of a page span can not come before its start"
      where
        truth = Map.lookup k runData
        test  = case rawPageLookup (ps Map.! i) (coerce k) of
          LookupEntryNotPresent       -> Nothing
          LookupEntry entry           -> Just entry
          LookupEntryOverflow entry n -> Just (first (concatOverflow n) entry)

        -- read remaining bytes for a multi-page value, and append it to the
        -- prefix we already have
        concatOverflow :: Word32 -> SerialisedValue -> SerialisedValue
        concatOverflow n (SerialisedValue v) =
            SerialisedValue $
            v <> RB.take (fromIntegral n) (mconcat $ fmap rawPageRawBytes overflowPages)
          where
            start = i + 1
            size  = j - i
            overflowPages = Map.elems $ Map.take size (Map.drop start ps)

    tabulate1Pre :: BinaryClassification -> Property -> Property
    tabulate1Pre  x = tabulate "Lookup classification: pre intra-page lookup"  [show x]

type TestRun = (Map Int RawPage, Bloom SerialisedKey, CompactIndex)

mkTestRun :: Map SerialisedKey (Entry SerialisedValue BlobSpan) -> TestRun
mkTestRun dat = (rawPages, b, cix)
  where
    nentries = NumEntries (Map.size dat)
    -- suggested range-finder precision is going to be @0@ anyway unless the
    -- input data is very big
    npages   = 0

    -- one-shot run construction
    (paccs, b, cix) = runST $ do
      racc <- Run.new nentries npages
      let kops = Map.toList dat
      mps <- traverse (uncurry (addFullKOp racc)) kops
      (mp, _ , bb, cixx, _) <- unsafeFinalise racc
      pure (mapMaybe (fmap fst) (mps ++ [mp]), bb, cixx)

    -- create a mapping of page numbers to raw pages, which we can use to do
    -- intra-page lookups on after first probing the bloom filter and index
    runBuilder = foldMap pageBuilder paccs
    runBytesS  = SBS.toShort $ BS.toStrict $ BB.toLazyByteString runBuilder
    runBytes   = case runBytesS of SBS.SBS ba -> ByteArray ba
    rawPages   = Map.fromList [ (i, makeRawPage runBytes (i * 4096))
                              | i <- [0 .. sizeofByteArray runBytes `div` 4096] ]

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

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

data InMemLookupData k v = InMemLookupData {
    -- | Data for constructing a run
    runData :: Map k (Entry v BlobSpan)
    -- | Lookups, with expected return values
  , lookups :: [k]
  }
  deriving stock (Show, Generic)
  deriving anyclass NFData

instance Arbitrary (InMemLookupData SerialisedKey SerialisedValue) where
  arbitrary = liftArbitrary2InMemLookupData genSerialisedKey genSerialisedValue
  shrink = liftShrink2InMemLookupData shrinkSerialisedKey shrinkSerialisedValue

instance Arbitrary1 (InMemLookupData SerialisedKey) where
  liftArbitrary = liftArbitrary2InMemLookupData genSerialisedKey

liftArbitrary2InMemLookupData :: Ord k => Gen k -> Gen v -> Gen (InMemLookupData k v)
liftArbitrary2InMemLookupData genKey genValue = do
    kops <- liftArbitrary2Map genKey (liftArbitrary genEntry)
              `suchThat` (\x -> Map.size x > 0)
    let runData = Map.mapMaybe id kops
    lookups <- sublistOf (Map.keys kops) >>= shuffle
    pure InMemLookupData{ runData, lookups }
  where
    genEntry = liftArbitrary2 genValue arbitrary

liftShrink2InMemLookupData :: Ord k => (k -> [k]) -> (v -> [v]) -> InMemLookupData k v -> [InMemLookupData k v]
liftShrink2InMemLookupData shrinkKey shrinkValue InMemLookupData{ runData, lookups } =
         [ InMemLookupData runData' lookups
         | runData' <- liftShrink2Map shrinkKey shrinkEntry runData ]
      ++ [ InMemLookupData runData lookups'
         | lookups' <- liftShrink shrinkKey lookups ]
    where
      shrinkEntry = liftShrink2 shrinkValue shrink

genSerialisedKey :: Gen SerialisedKey
genSerialisedKey = arbitrary `suchThat` (\k -> sizeofKey k >= 6)

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
                      ++ [ v' | let v' = SerialisedValue' (P.fromList $ replicate n 0), v' /= v ]
  | otherwise          = shrink v -- expensive, but thorough
  where n = sizeofValue v
