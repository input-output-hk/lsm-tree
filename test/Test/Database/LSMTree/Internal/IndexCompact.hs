{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Eta reduce" -}

module Test.Database.LSMTree.Internal.IndexCompact (tests) where

import           Control.DeepSeq (deepseq)
import           Control.Monad (foldM)
import           Control.Monad.ST (runST)
import           Control.Monad.State.Strict (MonadState (..), State, evalState,
                     get, put)
import           Data.Bit (Bit (..))
import qualified Data.Bit as BV
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Coerce (coerce)
import           Data.Foldable (Foldable (..))
import           Data.List.Split (chunksOf)
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Primitive.ByteArray (ByteArray (..), byteArrayFromList,
                     sizeofByteArray)
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VU (Vector (V_Word32))
import           Data.Word
import           Database.LSMTree.Extras
import           Database.LSMTree.Extras.Generators as Gen
import           Database.LSMTree.Internal.BitMath
import           Database.LSMTree.Internal.Entry (NumEntries (..))
import           Database.LSMTree.Internal.IndexCompact as Index
import           Database.LSMTree.Internal.IndexCompactAcc as Cons
import           Database.LSMTree.Internal.Serialise
import           Numeric (showHex)
import           Prelude hiding (max, min, pi)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.Orphans ()
import           Text.Printf (printf)

tests :: TestTree
tests = testGroup "Test.Database.LSMTree.Internal.IndexCompact" [
    testProperty "prop_distribution @KeyForIndexCompact" $
      prop_distribution @KeyForIndexCompact
  , testProperty "prop_searchMinMaxKeysAfterConstruction" $
      prop_searchMinMaxKeysAfterConstruction @KeyForIndexCompact 100
  , testProperty "prop_differentChunkSizesSameResults" $
      prop_differentChunkSizesSameResults @KeyForIndexCompact
  , testProperty "prop_singlesEquivMulti" $
      prop_singlesEquivMulti @KeyForIndexCompact
  , testGroup "(De)serialisation" [
        testGroup "test Chunks generator" [
            testProperty "Arbitrary satisfies invariant" $
              property . chunksInvariant
          , testProperty "Shrinking satisfies invariant" $
              property . all chunksInvariant . shrink @Chunks
        ]
      , testCase "index-2-clash" $ do
          let k1 = SerialisedKey' (VP.replicate 16 0x00)
          let k2 = SerialisedKey' (VP.replicate 16 0x11)
          let k3 = SerialisedKey' (VP.replicate 15 0x11 <> VP.replicate 1 0x12)
          let (chunks, index) = runST $ do
                ica <- Cons.new 0 16
                ch1 <- flip Cons.append ica $ AppendSinglePage k1 k2
                ch2 <- flip Cons.append ica $ AppendSinglePage k3 k3
                (mCh3, idx) <- unsafeEnd ica
                return (ch1 <> ch2 <> toList mCh3, idx)

          let expectedVersion :: [Word8]
              expectedVersion = word32toBytesLE 0x0000_0001
          let expectedPrimary :: [Word8]
              expectedPrimary = foldMap word32toBytesLE
                  -- 1. primary array: two pages (32 bits LE) + padding
                [ 0x0000_0000,  0x1111_1111
                ]
          let expectedRest :: [Word8]
              expectedRest = foldMap word32toBytesLE
                  -- 2. range finder: 2^0+1 = 2 entries 32 bit padding
                [ 0 {- offset = 0 -}, 2 {- numPages -}
                , 0 {- (padding to 64 bit) -}
                  -- 3. clash indicator: two pages, second one has bit
                , 0x0000_0002, 0
                  -- 4. larger-than-page: two pages, no bits
                , 0, 0
                  -- 5. clash map: maps k3 to page 1
                , 1, 0 {- size = 1 (64 bit LE) -}
                , 1, 16 {- page 1, key size 16 byte -}
                , 0x1111_1111, 0x1111_1111 {- k3 -}
                , 0x1111_1111, 0x1211_1111
                  -- 6.1 number of range finder bits (0..16) (64 bits LE)
                , 0, 0
                  -- 6.2 number of pages in the primary array (64 bits LE)
                , 2, 0
                  -- 6.3 number of keys (64bit LE)
                , 7, 0
                ]

          let header = buildBytes headerBuilder
          let primary = buildBytes (foldMap chunkBuilder chunks)
          let rest = buildBytes (finalBuilder (NumEntries 7) index)

          let comparison msg xs ys = unlines $
                  (msg <> ":")
                : zipWith (\x y -> x <> "  |  " <> y)
                    (showBytes xs <> repeat (replicate 17 '.'))
                    (showBytes ys)

          assertEqual (comparison "header" expectedVersion header)
            expectedVersion header
          assertEqual (comparison "primary" expectedPrimary primary)
            expectedPrimary primary
          assertEqual (comparison "rest" expectedRest rest)
            expectedRest rest

      , testProperty "prop_roundtrip_chunks" $
          prop_roundtrip_chunks
      , testProperty "prop_roundtrip" $
          prop_roundtrip @KeyForIndexCompact
      , testProperty "prop_total_deserialisation" $ withMaxSuccess 10000
          prop_total_deserialisation
      , testProperty "prop_total_deserialisation_whitebox" $ withMaxSuccess 10000
          prop_total_deserialisation_whitebox
      ]
  ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

--
-- Search
--

type CounterM a = State Int a

evalCounterM :: CounterM a -> Int -> a
evalCounterM = evalState

incrCounter :: CounterM Int
incrCounter = get >>= \c -> put (c+1) >> pure c

plusCounter :: Int -> CounterM Int
plusCounter n = get >>= \c -> put (c+n) >> pure c

-- | After construction, searching for the minimum/maximum key of every page
-- @pageNo@ returns the @pageNo@.
prop_searchMinMaxKeysAfterConstruction ::
     forall k. (SerialiseKey k, Show k, Ord k)
  => ChunkSize
  -> LogicalPageSummaries k
  -> Property
prop_searchMinMaxKeysAfterConstruction csize ps = eqMapProp real model
  where
    model = evalCounterM (foldM modelSearch Map.empty $ getPages ps) 0

    modelSearch :: Map k PageSpan -> LogicalPageSummary k -> CounterM (Map k PageSpan)
    modelSearch m = \case
        OnePageOneKey k       -> do
          c <- incrCounter
          pure $ Map.insert k (singlePage (PageNo c)) m
        OnePageManyKeys k1 k2 -> do
          c <- incrCounter
          pure $ Map.insert k1 (singlePage (PageNo c)) $ Map.insert k2 (singlePage (PageNo c)) m
        MultiPageOneKey k n -> do
          let incr = 1 + fromIntegral n
          c <- plusCounter incr
          pure $ if incr == 1 then Map.insert k (singlePage (PageNo c)) m
                              else Map.insert k (multiPage (PageNo c) (PageNo $ c + fromIntegral n)) m

    real = foldMap' realSearch (getPages ps)

    ic = fromPageSummaries (coerce csize) ps

    realSearch :: LogicalPageSummary k -> Map k PageSpan
    realSearch = \case
        OnePageOneKey k           -> Map.singleton k (search (serialiseKey k) ic)
        OnePageManyKeys k1 k2     -> Map.fromList [ (k1, search (serialiseKey k1) ic)
                                                  , (k2, search (serialiseKey k2) ic)]
        MultiPageOneKey k _       -> Map.singleton k (search (serialiseKey k) ic)

--
-- Construction
--

prop_differentChunkSizesSameResults ::
     SerialiseKey k
  => ChunkSize
  -> ChunkSize
  -> LogicalPageSummaries k
  -> Property
prop_differentChunkSizesSameResults csize1 csize2 ps =
    fromPageSummaries csize1 ps === fromPageSummaries csize2 ps

-- | Constructing an index using only 'appendSingle' is equivalent to using a
-- mix of 'appendSingle' and 'appendMulti'.
prop_singlesEquivMulti ::
     SerialiseKey k
  => ChunkSize
  -> ChunkSize
  -> LogicalPageSummaries k
  -> Property
prop_singlesEquivMulti csize1 csize2 ps = ic1 === ic2
  where
    apps1 = toAppends ps
    apps2 = concatMap toSingles apps1
    rfprec = coerce $ getRangeFinderPrecision ps
    ic1 = fromList rfprec (coerce csize1) apps1
    ic2 = fromListSingles rfprec (coerce csize2) apps2

    toSingles :: Append -> [(SerialisedKey, SerialisedKey)]
    toSingles (AppendSinglePage k1 k2) = [(k1, k2)]
    toSingles (AppendMultiPage k n)    = replicate (fromIntegral n + 1) (k, k)

-- | Distribution of generated test data
prop_distribution :: SerialiseKey k => LogicalPageSummaries k -> Property
prop_distribution ps =
    labelPages ps $ labelIndex (fromPageSummaries 100 ps) $ property True

-- Generate and serialise chunks directly.
-- This gives more direct shrinking of individual parts and has fewer invariants
-- (covering a larger space).
prop_roundtrip_chunks :: Chunks -> NumEntries -> Property
prop_roundtrip_chunks (Chunks chunks index) numEntries =
    counterexample (show (SBS.length sbs) <> " bytes") $
    counterexample ("header:\n" <> showBS bsVersion) $
    counterexample ("primary:\n" <> showBS bsPrimary) $
    counterexample ("rest:\n" <> showBS bsRest) $
      Right (numEntries, index) === fromSBS sbs
  where
    bsVersion = BB.toLazyByteString $ headerBuilder
    bsPrimary = BB.toLazyByteString $ foldMap chunkBuilder chunks
    bsRest = BB.toLazyByteString $ finalBuilder numEntries index
    sbs = SBS.toShort (LBS.toStrict (bsVersion <> bsPrimary <> bsRest))

    showBS = unlines . showBytes . LBS.unpack

-- Generate the compact index from logical pages.
prop_roundtrip :: SerialiseKey k => ChunkSize -> LogicalPageSummaries k -> NumEntries -> Property
prop_roundtrip csize ps numEntries =
    counterexample (show (SBS.length sbs) <> " bytes") $
    counterexample ("header:\n" <> showBS bsVersion) $
    counterexample ("primary:\n" <> showBS bsPrimary) $
    counterexample ("rest:\n" <> showBS bsRest) $
      Right (numEntries, index) === fromSBS sbs
  where
    index = fromPageSummaries csize ps

    (bsVersion, bsPrimary, bsRest) = writeIndexCompact numEntries csize ps
    sbs = SBS.toShort (LBS.toStrict (bsVersion <> bsPrimary <> bsRest))

    showBS = unlines . showBytes . LBS.unpack

prop_total_deserialisation :: [Word32] -> Property
prop_total_deserialisation word32s =
    let !(ByteArray ba) = byteArrayFromList word32s
    in case fromSBS (SBS.SBS ba) of
      Left err -> label err $ property True
      Right (numEntries, ic) -> label "parsed successfully" $ property $
        -- Just forcing the index is not enough. The underlying vectors might
        -- point to outside of the byte array, so we check they are valid.
        (numEntries, ic) `deepseq`
             vec32IsValid (icRangeFinder ic)
          && vec32IsValid (icPrimary ic)
          && bitVecIsValid (icClashes ic)
          && bitVecIsValid (icLargerThanPage ic)
  where
    vec32IsValid (VU.V_Word32 (VP.Vector off len ba)) =
      off >= 0 && len >= 0 && mul4 (off + len) <= sizeofByteArray ba
    bitVecIsValid (BV.BitVec off len ba) =
      off >= 0 && len >= 0 && ceilDiv8 (off + len) <= sizeofByteArray ba

prop_total_deserialisation_whitebox :: RFP -> Small Word16 -> Small Word16 -> [Word32] -> Property
prop_total_deserialisation_whitebox (RFP rfprec) numEntries numPages word32s =
    prop_total_deserialisation $
         [1]  -- version
      <> word32s  -- primary array, range finder, clash bits, LTP bits, clash map
      <> [0 | even (length word32s)]  -- padding
      <> [ fromIntegral rfprec, 0
         , fromIntegral numPages, 0
         , fromIntegral numEntries, 0
         ]

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

writeIndexCompact :: SerialiseKey k => NumEntries -> ChunkSize -> LogicalPageSummaries k -> (LBS.ByteString, LBS.ByteString, LBS.ByteString)
writeIndexCompact numEntries (ChunkSize csize) ps = runST $ do
    let rfprec = getRangeFinderPrecision ps
    ica <- Cons.new rfprec csize
    cs <- mapM (`append` ica) (toAppends ps)
    (c, index) <- unsafeEnd ica
    return
      ( BB.toLazyByteString headerBuilder
      , BB.toLazyByteString $ foldMap (foldMap chunkBuilder) cs
                           <> foldMap chunkBuilder c
      , BB.toLazyByteString $ finalBuilder numEntries index
      )

fromPageSummaries :: SerialiseKey k => ChunkSize -> LogicalPageSummaries k -> IndexCompact
fromPageSummaries (ChunkSize csize) ps =
    fromList rfprec csize (toAppends ps)
  where
    rfprec = getRangeFinderPrecision ps

fromList :: Int -> Int -> [Append] -> IndexCompact
fromList rfprec maxcsize apps = runST $ do
    ica <- new rfprec maxcsize
    mapM_ (`append` ica) apps
    (_, index) <- unsafeEnd ica
    pure index

-- | One-shot construction using only 'appendSingle'.
fromListSingles :: Int -> Int -> [(SerialisedKey, SerialisedKey)] -> IndexCompact
fromListSingles rfprec maxcsize apps = runST $ do
    ica <- new rfprec maxcsize
    mapM_ (`appendSingle` ica) apps
    (_, index) <- unsafeEnd ica
    pure index

labelIndex :: IndexCompact -> (Property -> Property)
labelIndex ic =
      QC.tabulate "# Clashes" [showPowersOf10 nclashes]
    . QC.tabulate "# Contiguous clash runs" [showPowersOf10 (length nscontig)]
    . QC.tabulate "Length of contiguous clash runs" (fmap (showPowersOf10 . snd) nscontig)
    . QC.tabulate "Contiguous clashes contain multi-page values" (fmap (show . fst) nscontig)
    . QC.classify (multiPageValuesClash ic) "Has clashing multi-page values"
  where nclashes       = Index.countClashes ic
        nscontig       = countContiguousClashes ic

multiPageValuesClash :: IndexCompact -> Bool
multiPageValuesClash ic
    | V.length (icClashes ic) < 3 = False
    | otherwise                   = V.any p $ V.zip4 v1 v2 v3 v4
  where
    -- note: @i = j - 1@ and @k = j + 1@. This gives us a local view of a
    -- triplet of contiguous LTP bits, and a C bit corresponding to the middle
    -- of the triplet.
    p (cj, ltpi, ltpj, ltpk) =
          -- two multi-page values border eachother
         unBit ltpi && not (unBit ltpj) && unBit ltpk
         -- and they clash
      && unBit cj
    v1 = V.tail (icClashes ic)
    v2 = icLargerThanPage ic
    v3 = V.tail v2
    v4 = V.tail v3

-- Returns the number of entries and whether any multi-page values were in the
-- contiguous clashes
countContiguousClashes :: IndexCompact -> [(Bool, Int)]
countContiguousClashes ic = actualContigClashes
  where
    -- filtered is a list of maximal sub-vectors that have only only contiguous
    -- clashes
    zipped    = V.zip (icClashes ic) (icLargerThanPage ic)
    grouped   = V.groupBy (\x y -> fst x == fst y) zipped
    filtered  = filter (V.all (\(c, _ltp) -> c == Bit True)) grouped
    -- clashes that are part of a multi-page value shouldn't be counted towards
    -- the total number of /actual/ clashes. We only care about /actual/ clashes
    -- if they total more than 1 (otherwise it's just a single clash)
    actualContigClashes = filter (\(_, n) -> n > 1) $
                          fmap (\v -> (countLTP v > 0, countC v - countLTP v)) filtered
    countC              = V.length
    countLTP            = BV.countBits . V.map snd

-- | Point-wise equality test for two maps, returning counterexamples for each
-- mismatch.
eqMapProp :: (Ord k, Eq v, Show k, Show v) => Map k v -> Map k v -> Property
eqMapProp m1 m2 = conjoin . Map.elems $
    Merge.merge
      (Merge.mapMissing onlyLeft)
      (Merge.mapMissing onlyRight)
      (Merge.zipWithMatched both)
      m1
      m2
  where
    onlyLeft k x = flip counterexample (property False) $
      printf "Key-value pair only on the left, (k, x) = (%s, %s)" (show k) (show x)
    onlyRight k y = flip counterexample (property False) $
      printf "Key-value pair only on the right, (k, y) = (%s, %s)" (show k) (show y)
    both k x y = flip counterexample (property (x == y)) $
      printf "Mismatch between x and y, (k, x, y) = (%s, %s, %s)" (show k) (show x) (show y)

showBytes :: [Word8] -> [String]
showBytes = map (unwords . map (foldMap showByte) . chunksOf 4) . chunksOf 8

showByte :: Word8 -> String
showByte b = let str = showHex b "" in replicate (2 - length str) '0' <> str

buildBytes :: BB.Builder -> [Word8]
buildBytes = LBS.unpack . BB.toLazyByteString

word32toBytesLE :: Word32 -> [Word8]
word32toBytesLE = take 4 . map fromIntegral . iterate (`div` 256)

data Chunks = Chunks [Chunk] IndexCompact
  deriving (Show)

-- | The concatenated chunks must correspond to the primary array of the index.
-- Apart from that, the only invariant we make sure to uphold is that the length
-- of the vectors match each other (or 'fcRangeFinderPrecision'), as this is
-- required for correct deserialisation.
-- These invariants do not guarantee that the 'IndexCompact' is valid in other
-- ways (e.g. can successfully be queried).
chunksInvariant :: Chunks -> Bool
chunksInvariant (Chunks chunks IndexCompact {..}) =
       rfprecInvariant (unsafeMkRFP icRangeFinderPrecision)
    && VU.length icPrimary == sum (map (VU.length . cPrimary) chunks)
    && VU.length icClashes == VU.length icPrimary
    && VU.length icLargerThanPage == VU.length icPrimary
    && VU.length icRangeFinder == 2 ^ icRangeFinderPrecision + 1

instance Arbitrary Chunks where
  arbitrary = do
    chunks <- map VU.fromList <$> arbitrary
    let icPrimary = mconcat chunks
    let numPages = VU.length icPrimary

    RFP icRangeFinderPrecision <- arbitrary
    icRangeFinder <- VU.fromList <$> vector (2 ^ icRangeFinderPrecision + 1)
    icClashes <- VU.fromList . map Bit <$> vector numPages
    icLargerThanPage <- VU.fromList . map Bit <$> vector numPages
    icTieBreaker <- arbitrary
    return (Chunks (map Chunk chunks) IndexCompact {..})

  shrink (Chunks chunks index) =
    -- shrink range finder bits
    [ Chunks chunks index
        { icRangeFinder = VU.take (2 ^ rfprec' + 1) (icRangeFinder index)
        , icRangeFinderPrecision = rfprec'
        }
    | RFP rfprec' <- shrink (unsafeMkRFP (icRangeFinderPrecision index))
    ] ++
    -- shrink number of pages
    [ Chunks (map Chunk chunks') index
        { icPrimary = primary'
        , icClashes = VU.slice 0 numPages' (icClashes index)
        , icLargerThanPage = VU.slice 0 numPages' (icLargerThanPage index)
        }
    | chunks' <- shrink (map cPrimary chunks)
    , let primary' = mconcat chunks'
    , let numPages' = VU.length primary'
    ] ++
    -- shrink tie breaker
    [ Chunks chunks index
        { icTieBreaker = tieBreaker'
        }
    | tieBreaker' <- shrink (icTieBreaker index)
    ]
