{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Database.LSMTree.Generators (
    -- * WriteBuffer
    genWriteBuffer
  , shrinkWriteBuffer
  , writeBufferInvariant
    -- * WithSerialised
  , WithSerialised (..)
    -- * UTxO keys
  , UTxOKey (..)
    -- * Range-finder precision
  , RFPrecision (..)
  , rfprecInvariant
    -- * A (logical\/true) page
    -- ** A true page
  , TruePageSummary (..)
  , flattenLogicalPageSummary
    -- ** A logical page
  , LogicalPageSummary (..)
  , shrinkLogicalPageSummary
  , toAppend
    -- * Sequences of (logical\/true) pages
  , Pages (..)
  , optimiseRFPrecision
    -- ** Sequences of true pages
  , TruePageSummaries
  , flattenLogicalPageSummaries
    -- ** Sequences of logical pages
  , LogicalPageSummaries
  , toAppends
  , labelPages
  , shrinkPages
  , genPages
  , mkPages
  , pagesInvariant
    -- * Chunking size
  , ChunkSize (..)
  , chunkSizeInvariant
    -- * Serialised keys\/values\/blobs
  , genRawBytes
  , genRawBytesN
  , genRawBytesSized
  , packRawBytesPinnedOrUnpinned
  ) where

import           Control.DeepSeq (NFData)
import           Data.Bifunctor (bimap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Data.Containers.ListUtils (nubOrd)
import           Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Vector.Primitive as P
import           Data.WideWord.Word256 (Word256 (..))
import           Data.Word
import           Database.LSMTree.Common (Range (..))
import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import           Database.LSMTree.Internal.Entry (Entry (..), NumEntries (..))
import           Database.LSMTree.Internal.Run.Index.Compact (PageNo (..),
                     rangeFinderPrecisionBounds, suggestRangeFinderPrecision)
import           Database.LSMTree.Internal.Run.Index.Compact.Construction
                     (Append (..))
import           Database.LSMTree.Internal.Serialise
import qualified Database.LSMTree.Internal.Serialise.Class as S.Class
import           Database.LSMTree.Internal.Serialise.RawBytes as RB
import           Database.LSMTree.Internal.WriteBuffer (WriteBuffer (..))
import qualified Database.LSMTree.Internal.WriteBuffer as WB
import qualified Database.LSMTree.Monoidal as Monoidal
import qualified Database.LSMTree.Normal as Normal
import           Database.LSMTree.Util
import           Database.LSMTree.Util.Orphans ()
import           GHC.Generics (Generic)
import           System.Random (Uniform)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Arbitrary (..), Arbitrary1 (..),
                     Arbitrary2 (..), Gen, Property, frequency, oneof)
import           Test.QuickCheck.Gen (genDouble)
import           Test.QuickCheck.Instances ()

{-------------------------------------------------------------------------------
  Common LSMTree types
-------------------------------------------------------------------------------}

instance (Arbitrary v, Arbitrary blob) => Arbitrary (Normal.Update v blob) where
  arbitrary = QC.arbitrary2
  shrink = QC.shrink2

instance Arbitrary2 Normal.Update where
  liftArbitrary2 genVal genBlob = frequency
    [ (10, Normal.Insert <$> genVal <*> liftArbitrary genBlob)
    , (1, pure Normal.Delete)
    ]

  liftShrink2 shrinkVal shrinkBlob = \case
    Normal.Insert v blob ->
        Normal.Delete
      : map (uncurry Normal.Insert)
            (liftShrink2 shrinkVal (liftShrink shrinkBlob) (v, blob))
    Normal.Delete ->
      []

instance (Arbitrary v) => Arbitrary (Monoidal.Update v) where
  arbitrary = QC.arbitrary1
  shrink = QC.shrink1

instance Arbitrary1 Monoidal.Update where
  liftArbitrary genVal = frequency
    [ (10, Monoidal.Insert <$> genVal)
    , (5, Monoidal.Mupsert <$> genVal)
    , (1, pure Monoidal.Delete)
    ]

  liftShrink shrinkVal = \case
    Monoidal.Insert v  -> Monoidal.Delete : map Monoidal.Insert (shrinkVal v)
    Monoidal.Mupsert v -> Monoidal.Insert v : map Monoidal.Mupsert (shrinkVal v)
    Monoidal.Delete    -> []

instance Arbitrary k => Arbitrary (Range k) where
  arbitrary = oneof
    [ FromToExcluding <$> arbitrary <*> arbitrary
    , FromToIncluding <$> arbitrary <*> arbitrary
    ]

  shrink (FromToExcluding f t) =
    uncurry FromToExcluding <$> shrink (f, t)
  shrink (FromToIncluding f t) =
    uncurry FromToIncluding <$> shrink (f, t)

{-------------------------------------------------------------------------------
  Entry
-------------------------------------------------------------------------------}

instance (Arbitrary v, Arbitrary blob) => Arbitrary (Entry v blob) where
  arbitrary = QC.arbitrary2
  shrink = QC.shrink2

instance Arbitrary2 Entry where
  liftArbitrary2 genVal genBlob = frequency
    [ (10, Insert <$> genVal)
    , (1,  InsertWithBlob <$> genVal <*> genBlob)
    , (1,  Mupdate <$> genVal)
    , (1,  pure Delete)
    ]

  liftShrink2 shrinkVal shrinkBlob = \case
    Insert v           -> Delete : (Insert <$> shrinkVal v)
    InsertWithBlob v b -> [Delete, Insert v]
                       ++ fmap (uncurry InsertWithBlob)
                            (liftShrink2 shrinkVal shrinkBlob (v, b))
    Mupdate v          -> Delete : Insert v : (Mupdate <$> shrinkVal v)
    Delete             -> []

{-------------------------------------------------------------------------------
  WriteBuffer
-------------------------------------------------------------------------------}

instance (Arbitrary v, Arbitrary blob, SerialiseValue v, SerialiseValue blob)
      => Arbitrary (WriteBuffer ByteString v blob) where
  arbitrary = genWriteBuffer arbitrary arbitrary
  shrink = shrinkWriteBuffer shrink shrink

-- | We cannot implement 'Arbitrary2' since we have constraints on the type
-- parameters.
-- Assuming that keys are bytestrings gives us control over their length,
-- so we can ensure to always generate suitable keys for a compact index
-- (at least 6 bytes).
genWriteBuffer ::
     (SerialiseValue v, SerialiseValue blob)
  => Gen v
  -> Gen blob
  -> Gen (WriteBuffer ByteString v blob)
genWriteBuffer genVal genBlob = fmap fromKOps genKOps
  where
    -- minimum length 6 bytes
    genKey = (<>) <$> (BS.pack <$> QC.vector 6) <*> arbitrary
    genKOps = QC.listOf (liftArbitrary2 genKey (liftArbitrary2 genVal genBlob))

shrinkWriteBuffer ::
     (SerialiseValue v, SerialiseValue blob)
  => (v -> [v]) -> (blob -> [blob])
  -> WriteBuffer ByteString v blob
  -> [WriteBuffer ByteString v blob]
shrinkWriteBuffer shrinkVal shrinkBlob = map fromKOps . shrinkKOps . toKOps
  where
    -- minimum length 6 bytes
    shrinkKey k = fmap (BS.take 6 k <>) (shrink (BS.drop 6 k))
    shrinkKOps =
      liftShrink (liftShrink2 shrinkKey (liftShrink2 shrinkVal shrinkBlob))

fromKOps ::
     (SerialiseKey k, SerialiseValue v, SerialiseValue blob)
  => [(k, Entry v blob)]
  -> WriteBuffer k v blob
fromKOps = WB . Map.fromList . map serialiseKOp
  where
    serialiseKOp = bimap serialiseKey (bimap serialiseValue serialiseBlob)

toKOps ::
     (SerialiseKey k, SerialiseValue v, SerialiseValue blob)
  => WriteBuffer k v blob
  -> [(k, Entry v blob)]
toKOps = map deserialiseKOp . Map.assocs . WB.unWB
  where
    deserialiseKOp =
      bimap deserialiseKey (bimap deserialiseValue deserialiseBlob)

writeBufferInvariant :: WriteBuffer k v blob -> Bool
writeBufferInvariant (WB wb) = all isValidKey (Map.keys wb)
  where
    isValidKey k = sizeofKey k >= 6

{-------------------------------------------------------------------------------
  WithSerialised
-------------------------------------------------------------------------------}

-- | Cache serialised keys
--
-- Also useful for failing tests that have keys as inputs, because the printed
-- 'WithSerialised' values will show both keys and their serialised form.
data WithSerialised k = TestKey k SerialisedKey
  deriving Show

instance Eq k => Eq (WithSerialised k) where
  TestKey k1 _ == TestKey k2 _ = k1 == k2

instance Ord k => Ord (WithSerialised k) where
  TestKey k1 _ `compare` TestKey k2 _ = k1 `compare` k2

instance (Arbitrary k, SerialiseKey k) => Arbitrary (WithSerialised k) where
  arbitrary = do
    x <- arbitrary
    pure $ TestKey x (serialiseKey x)
  shrink (TestKey k _) = [TestKey k' (serialiseKey k') | k' <- shrink k]

instance SerialiseKey k => SerialiseKey (WithSerialised k) where
  serialiseKey (TestKey _ (SerialisedKey bytes)) = bytes
  deserialiseKey bytes = TestKey (S.Class.deserialiseKey bytes) (SerialisedKey bytes)

{-------------------------------------------------------------------------------
  UTxO keys
-------------------------------------------------------------------------------}

-- | A model of a UTxO key (256-bit hash)
newtype UTxOKey = UTxOKey Word256
  deriving stock (Show, Generic)
  deriving newtype ( Eq, Ord, NFData
                   , SerialiseKey
                   , Num, Enum, Real, Integral
                   )
  deriving anyclass Uniform

instance Arbitrary UTxOKey where
  arbitrary = UTxOKey <$>
      (Word256 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
  shrink (UTxOKey w256) = [
        UTxOKey w256'
      | let i256 = toInteger w256
      , i256' <- shrink i256
      , toInteger (minBound :: Word256) <= i256'
      , toInteger (maxBound :: Word256) >= i256'
      , let w256' = fromIntegral i256'
      ]

{-------------------------------------------------------------------------------
  Range-finder precision
-------------------------------------------------------------------------------}

newtype RFPrecision = RFPrecision Int
  deriving stock (Show, Generic)
  deriving newtype Num
  deriving anyclass NFData

instance Arbitrary RFPrecision where
  arbitrary = RFPrecision <$> QC.chooseInt (rfprecLB, rfprecUB)
    where (rfprecLB, rfprecUB) = rangeFinderPrecisionBounds
  shrink (RFPrecision x) =
      [RFPrecision x' | x' <- shrink x , rfprecInvariant (RFPrecision x')]

rfprecInvariant :: RFPrecision -> Bool
rfprecInvariant (RFPrecision x) = x >= rfprecLB && x <= rfprecUB
  where (rfprecLB, rfprecUB) = rangeFinderPrecisionBounds

{-------------------------------------------------------------------------------
  Other number newtypes
-------------------------------------------------------------------------------}

instance Arbitrary PageNo where
  arbitrary = coerce (arbitrary @(QC.NonNegative Int))
  shrink = coerce (shrink @(QC.NonNegative Int))

instance Arbitrary NumEntries where
  arbitrary = coerce (arbitrary @(QC.NonNegative Int))
  shrink = coerce (shrink @(QC.NonNegative Int))

{-------------------------------------------------------------------------------
  True page
-------------------------------------------------------------------------------}

-- | A summary of min/max information for keys on a /true/ page.
--
-- A ture page corresponds directly to a disk page. See 'LogicalPageSummary' for
-- contrast.
data TruePageSummary k = TruePageSummary { tpsMinKey :: k, tpsMaxKey :: k }

flattenLogicalPageSummary :: LogicalPageSummary k -> [TruePageSummary k]
flattenLogicalPageSummary = \case
    OnePageOneKey k       -> [TruePageSummary k k]
    OnePageManyKeys k1 k2 -> [TruePageSummary k1 k2]
    MultiPageOneKey k n   -> replicate (fromIntegral n+1) (TruePageSummary k k)

{-------------------------------------------------------------------------------
  Logical page
-------------------------------------------------------------------------------}

-- | A summary of min/max information for keys on a /logical/ page.
--
-- A key\/operation pair can fit onto a single page, or the operation is so
-- large that its bytes flow over into subsequent pages. A logical page makes
-- this overflow explicit. Making these cases explicit in the representation
-- makes generating and shrinking test cases easier.
data LogicalPageSummary k =
    OnePageOneKey   k
  | OnePageManyKeys k k
  | MultiPageOneKey k Word32 -- ^ number of overflow pages
  deriving stock (Show, Generic, Functor)
  deriving anyclass NFData

toAppend :: LogicalPageSummary SerialisedKey -> Append
toAppend (OnePageOneKey k)       = AppendSinglePage k k
toAppend (OnePageManyKeys k1 k2) = AppendSinglePage k1 k2
toAppend (MultiPageOneKey k n)   = AppendMultiPage k n

shrinkLogicalPageSummary :: Arbitrary k => LogicalPageSummary k -> [LogicalPageSummary k]
shrinkLogicalPageSummary = \case
    OnePageOneKey k       -> OnePageOneKey <$> shrink k
    OnePageManyKeys k1 k2 -> OnePageManyKeys <$> shrink k1 <*> shrink k2
    MultiPageOneKey k n   -> [MultiPageOneKey k' n | k' <- shrink k]
                          <> [MultiPageOneKey k n' | n' <- shrink n]

{-------------------------------------------------------------------------------
  Sequences of (logical\/true) pages
-------------------------------------------------------------------------------}

-- | Sequences of (logical\/true) pages
--
-- INVARIANT: The sequence consists of multiple pages in sorted order (keys are
-- sorted within a page and across pages). Pages are partitioned, meaning all
-- keys inside a page have the same range-finder bits.
data Pages fp k = Pages {
    getRangeFinderPrecision :: RFPrecision
  , getPages                :: [fp k]
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass NFData

class TrueNumberOfPages fp where
  trueNumberOfPages :: Pages fp k -> Int

instance TrueNumberOfPages LogicalPageSummary where
  trueNumberOfPages :: LogicalPageSummaries k -> Int
  trueNumberOfPages = length . getPages . flattenLogicalPageSummaries

instance TrueNumberOfPages TruePageSummary where
  trueNumberOfPages :: TruePageSummaries k -> Int
  trueNumberOfPages = length . getPages

optimiseRFPrecision :: TrueNumberOfPages fp => Pages fp k -> Pages fp k
optimiseRFPrecision ps = ps {
      getRangeFinderPrecision = coerce $ suggestRangeFinderPrecision (trueNumberOfPages ps)
    }

{-------------------------------------------------------------------------------
  Sequences of true pages
-------------------------------------------------------------------------------}

type TruePageSummaries    k = Pages TruePageSummary k

flattenLogicalPageSummaries :: LogicalPageSummaries k -> TruePageSummaries k
flattenLogicalPageSummaries (Pages f ps) = Pages f (concatMap flattenLogicalPageSummary ps)

{-------------------------------------------------------------------------------
  Sequences of logical pages
-------------------------------------------------------------------------------}

type LogicalPageSummaries k = Pages LogicalPageSummary k

toAppends :: SerialiseKey k => LogicalPageSummaries k -> [Append]
toAppends (Pages _ ps) = fmap (toAppend . fmap serialiseKey) ps

--
-- Labelling
--

labelPages :: LogicalPageSummaries k -> (Property -> Property)
labelPages ps =
      QC.tabulate "RFPrecision: optimal" [show suggestedRfprec]
    . QC.tabulate "RFPrecision: actual" [show actualRfprec]
    . QC.tabulate "RFPrecision: |optimal-actual|" [show dist]
    . QC.tabulate "# True pages" [showPowersOf10 nTruePages]
    . QC.tabulate "# Logical pages" [showPowersOf10 nLogicalPages]
    . QC.tabulate "# OnePageOneKey logical pages" [showPowersOf10 n1]
    . QC.tabulate "# OnePageManyKeys logical pages" [showPowersOf10 n2]
    . QC.tabulate "# MultiPageOneKey logical pages" [showPowersOf10 n3]
  where
    nLogicalPages = length $ getPages ps
    nTruePages = trueNumberOfPages ps
    actualRfprec = getRangeFinderPrecision ps
    suggestedRfprec = getRangeFinderPrecision $ optimiseRFPrecision ps
    dist = abs (suggestedRfprec - actualRfprec)

    (n1,n2,n3) = counts (getPages ps)

    counts :: [LogicalPageSummary k] -> (Int, Int, Int)
    counts []       = (0, 0, 0)
    counts (lp:lps) = let (x, y, z) = counts lps
                      in case lp of
                        OnePageOneKey{}   -> (x+1, y, z)
                        OnePageManyKeys{} -> (x, y+1, z)
                        MultiPageOneKey{} -> (x, y, z+1)

--
-- Generation and shrinking
--

instance (Arbitrary k, Ord k, SerialiseKey k)
      => Arbitrary (LogicalPageSummaries k) where
  arbitrary = genPages 0.03 (QC.choose (0, 16))
  shrink = shrinkPages

shrinkPages ::
     (Arbitrary k, Ord k, SerialiseKey k)
  => LogicalPageSummaries k
  -> [LogicalPageSummaries k]
shrinkPages (Pages rfprec ps) = [
      Pages rfprec ps'
    | ps' <- QC.shrinkList shrinkLogicalPageSummary ps, pagesInvariant (Pages rfprec ps')
    ] <> [
      Pages rfprec' ps
    | rfprec' <- shrink rfprec, pagesInvariant (Pages rfprec' ps)
    ]

genPages ::
     (Arbitrary k, Ord k, SerialiseKey k)
  => Double -- ^ Probability of a value being larger-than-page
  -> Gen Word32 -- ^ Number of overflow pages for a larger-than-page value
  -> Gen (LogicalPageSummaries k)
genPages p genN = do
    rfprec <- arbitrary
    ks <- arbitrary
    mkPages p genN rfprec ks

mkPages ::
     forall k. (Ord k, SerialiseKey k)
  => Double -- ^ Probability of a value being larger-than-page
  -> Gen Word32 -- ^ Number of overflow pages for a larger-than-page value
  -> RFPrecision
  -> [k]
  -> Gen (LogicalPageSummaries k)
mkPages p genN rfprec@(RFPrecision n) =
    fmap (Pages rfprec) . go . nubOrd . sort
  where
    go :: [k] -> Gen [LogicalPageSummary k]
    go []          = pure []
    go [k]         = do b <- largerThanPage
                        if b then pure . MultiPageOneKey k <$> genN else pure [OnePageOneKey k]
                   -- the min and max key are allowed to be the same
    go  (k1:k2:ks) = do b <- largerThanPage
                        if | b
                           -> (:) <$> (MultiPageOneKey k1 <$> genN) <*> go (k2 : ks)
                           | keyTopBits16 n (serialiseKey k1) == keyTopBits16 n (serialiseKey k2)
                           -> (OnePageManyKeys k1 k2 :) <$> go ks
                           | otherwise
                           -> (OnePageOneKey k1 :) <$>  go (k2 : ks)

    largerThanPage :: Gen Bool
    largerThanPage = genDouble >>= \x -> pure (x < p)

pagesInvariant :: (Ord k, SerialiseKey k) => LogicalPageSummaries k -> Bool
pagesInvariant (Pages (RFPrecision rfprec) ps0) =
       sort ks   == ks
    && nubOrd ks == ks
    && all partitioned ps0
  where
    ks = flatten ps0
    partitioned = \case
      OnePageOneKey _       -> True
      OnePageManyKeys k1 k2 -> keyTopBits16 rfprec (serialiseKey k1)
                               == keyTopBits16 rfprec (serialiseKey k2)
      MultiPageOneKey _ _   -> True

    flatten :: Eq k => [LogicalPageSummary k] -> [k]
    flatten []            = []
                          -- the min and max key are allowed to be the same
    flatten (p:ps) = case p of
      OnePageOneKey k       -> k : flatten ps
      OnePageManyKeys k1 k2 -> k1 : k2 : flatten ps
      MultiPageOneKey k _   -> k : flatten ps

{-------------------------------------------------------------------------------
  Chunking size
-------------------------------------------------------------------------------}

newtype ChunkSize = ChunkSize Int
  deriving stock Show
  deriving newtype Num

instance Arbitrary ChunkSize where
  arbitrary = ChunkSize <$> QC.chooseInt (chunkSizeLB, chunkSizeUB)
  shrink (ChunkSize csize) = [
        ChunkSize csize'
      | csize' <- shrink csize
      , chunkSizeInvariant (ChunkSize csize')
      ]

chunkSizeLB, chunkSizeUB :: Int
chunkSizeLB = 1
chunkSizeUB = 20

chunkSizeInvariant :: ChunkSize -> Bool
chunkSizeInvariant (ChunkSize csize) = chunkSizeLB <= csize && csize <= chunkSizeUB

{-------------------------------------------------------------------------------
  Serialised keys/values/blobs
-------------------------------------------------------------------------------}

instance Arbitrary RawBytes where
  arbitrary = genRawBytes >>= genSlice
  shrink rb = shrinkRawBytes rb ++ shrinkSlice rb

genRawBytesN :: Int -> Gen RawBytes
genRawBytesN n =
    packRawBytesPinnedOrUnpinned <$> arbitrary <*> QC.vectorOf n arbitrary

genRawBytes :: Gen RawBytes
genRawBytes =
    packRawBytesPinnedOrUnpinned <$> arbitrary <*> QC.listOf arbitrary

genRawBytesSized :: Int -> Gen RawBytes
genRawBytesSized n = QC.resize n genRawBytes

packRawBytesPinnedOrUnpinned :: Bool -> [Word8] -> RawBytes
packRawBytesPinnedOrUnpinned False = RB.pack
packRawBytesPinnedOrUnpinned True  = \ws ->
    let len = length ws in
    RB.RawBytes $ P.Vector 0 len $ BA.runByteArray $ do
      mba <- BA.newPinnedByteArray len
      sequence_ [ BA.writeByteArray mba i w | (i, w) <- zip [0..] ws ]
      return mba

shrinkRawBytes :: RawBytes -> [RawBytes]
shrinkRawBytes (RawBytes pvec) = [ RawBytes (P.fromList ws)
                                 | ws <- QC.shrink (P.toList pvec) ]

genSlice :: RawBytes -> Gen RawBytes
genSlice (RawBytes pvec) = do
    n <- QC.chooseInt (0, P.length pvec)
    m <- QC.chooseInt (0, P.length pvec - n)
    pure $ RawBytes (P.slice m n pvec)

shrinkSlice :: RawBytes -> [RawBytes]
shrinkSlice (RawBytes pvec) =
    [ RawBytes (P.slice m n pvec)
    | n <- QC.shrink (P.length pvec)
    , m <- QC.shrink (P.length pvec - n)
    ]

deriving newtype instance Arbitrary SerialisedKey

deriving newtype instance Arbitrary SerialisedValue

instance Arbitrary SerialisedBlob where
  arbitrary = SerialisedBlob <$> genRawBytes
  shrink (SerialisedBlob rb) = SerialisedBlob <$> shrinkRawBytes rb

{-------------------------------------------------------------------------------
  BlobRef
-------------------------------------------------------------------------------}

instance Arbitrary BlobSpan where
  arbitrary = BlobSpan <$> arbitrary <*> arbitrary
  shrink (BlobSpan x y) = BlobSpan <$> shrink x <*> shrink y
