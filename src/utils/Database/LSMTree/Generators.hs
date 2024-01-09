{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Database.LSMTree.Generators (
    -- * WithSerialised
    WithSerialised (..)
    -- * UTxO keys
  , UTxOKey (..)
    -- * Range-finder precision
  , RFPrecision (..)
  , rfprecInvariant
    -- * Page
  , Page (..)
  , fromPage
  , shrinkPage
    -- * Pages (non-partitioned)
  , Pages (..)
  , fromPages
  , shrinkPages
  , genPages
  , mkPages
  , pagesInvariant
  , labelPages
    -- * Chunking size
  , ChunkSize (..)
  , chunkSizeInvariant
  ) where

import           Control.DeepSeq (NFData)
import           Data.Containers.ListUtils (nubOrd)
import           Data.List (sort)
import           Data.WideWord.Word256 (Word256 (..))
import           Data.Word
import           Database.LSMTree.Internal.Run.BloomFilter (Hashable (..))
import           Database.LSMTree.Internal.Run.Index.Compact (Append (..),
                     rangeFinderPrecisionBounds, suggestRangeFinderPrecision)
import           Database.LSMTree.Internal.Serialise (Serialise (..),
                     SerialisedKey, topBits16)
import           Database.LSMTree.Util.Orphans ()
import           GHC.Generics (Generic)
import           System.Random (Uniform)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Arbitrary (..), Gen, Property)
import           Test.QuickCheck.Gen (genDouble)
import           Text.Printf (printf)

{-------------------------------------------------------------------------------
  WithSerialised
-------------------------------------------------------------------------------}

-- | Cach serialised keys
--
-- Also useful for failing tests that have keys as inputs, because the printed
-- 'WithSerialised' values will show both keys and their serialised form.
data WithSerialised k = TestKey k SerialisedKey
  deriving Show

instance Eq k => Eq (WithSerialised k) where
  TestKey k1 _ == TestKey k2 _ = k1 == k2

instance Ord k => Ord (WithSerialised k) where
  TestKey k1 _ `compare` TestKey k2 _ = k1 `compare` k2

instance (Arbitrary k, Serialise k) => Arbitrary (WithSerialised k) where
  arbitrary = do
    x <- arbitrary
    pure $ TestKey x (serialise x)
  shrink (TestKey k _) = [TestKey k' (serialise k') | k' <- shrink k]

instance Serialise (WithSerialised k) where
  serialise (TestKey _ skey) = skey

{-------------------------------------------------------------------------------
  UTxO keys
-------------------------------------------------------------------------------}

-- | A model of a UTxO key (256-bit hash)
newtype UTxOKey = UTxOKey Word256
  deriving stock (Show, Generic)
  deriving newtype ( Eq, Ord, NFData
                   , Hashable, Serialise
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
  Page
-------------------------------------------------------------------------------}

data Page k =
    OneKey k
  | ManyKeys k k
  | LargerThanPage k Word32
  deriving stock (Show, Generic, Functor)
  deriving anyclass NFData

fromPage :: Page SerialisedKey -> Append
fromPage (OneKey k)           = AppendSinglePage k k
fromPage (ManyKeys k1 k2)     = AppendSinglePage k1 k2
fromPage (LargerThanPage k n) = AppendMultiPage k n

shrinkPage :: Arbitrary k => Page k -> [Page k]
shrinkPage = \case
    OneKey k           -> OneKey <$> shrink k
    ManyKeys k1 k2     -> ManyKeys <$> shrink k1 <*> shrink k2
    LargerThanPage k n -> [LargerThanPage k' n | k' <- shrink k]
                       <> [LargerThanPage k n' | n' <- shrink n]

{-------------------------------------------------------------------------------
  Pages (partitioned)
-------------------------------------------------------------------------------}

-- | We model a disk page in a run as a pair of its minimum and maximum key.
--
-- A run consists of multiple pages in sorted order, and keys are unique. Pages
-- are partitioned, meaning all keys inside a page have the same range-finder
-- bits. A run can not be empty, and a page can not be empty.
data Pages k = Pages {
    getRangeFinderPrecision :: RFPrecision
  , getPages                :: [Page k]
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass NFData

fromPages :: Serialise k => [Page k] -> [Append]
fromPages = fmap (fromPage . fmap serialise)

instance (Arbitrary k, Ord k, Serialise k) => Arbitrary (Pages k) where
  arbitrary = genPages 0.01 (QC.choose (0, 100))
  shrink = shrinkPages

shrinkPages :: (Arbitrary k, Ord k, Serialise k) => Pages k -> [Pages k]
shrinkPages (Pages rfprec ps) = [
      Pages rfprec ps'
    | ps' <- QC.shrinkList shrinkPage ps, pagesInvariant (Pages rfprec ps')
    ] <> [
      Pages rfprec' ps
    | rfprec' <- shrink rfprec, pagesInvariant (Pages rfprec' ps)
    ]

genPages ::
     (Arbitrary k, Ord k, Serialise k)
  => Double -- ^ Probability of a value being larger-than-page
  -> Gen Word32 -- ^ Number of overflow pages for a larger-than-page value
  -> Gen (Pages k)
genPages p genN = do
    rfprec <- arbitrary
    ks <- arbitrary
    mkPages p genN rfprec ks

mkPages ::
     forall k. (Ord k, Serialise k)
  => Double -- ^ Probability of a value being larger-than-page
  -> Gen Word32 -- ^ Number of overflow pages for a larger-than-page value
  -> RFPrecision
  -> [k]
  -> Gen (Pages k)
mkPages p genN rfprec@(RFPrecision n) =
    fmap (Pages rfprec) . go . nubOrd . sort
  where
    go :: [k] -> Gen [Page k]
    go []          = pure []
    go [k]         = do b <- largerThanPage
                        if b then pure . LargerThanPage k <$> genN else pure [OneKey k]
                   -- the min and max key are allowed to be the same
    go  (k1:k2:ks) = do b <- largerThanPage
                        if | b
                           -> (:) <$> (LargerThanPage k1 <$> genN) <*> go (k2 : ks)
                           | topBits16 n (serialise k1) == topBits16 n (serialise k2)
                           -> (ManyKeys k1 k2 :) <$> go ks
                           | otherwise
                           -> (OneKey k1 :) <$>  go (k2 : ks)

    largerThanPage :: Gen Bool
    largerThanPage = genDouble >>= \x -> pure (x < p)

pagesInvariant :: (Ord k, Serialise k) => Pages k -> Bool
pagesInvariant (Pages (RFPrecision rfprec) ps0) =
       sort ks   == ks
    && nubOrd ks == ks
    && all partitioned ps0
  where
    ks = flatten ps0
    partitioned = \case
      OneKey _           -> True
      ManyKeys k1 k2     -> topBits16 rfprec (serialise k1) == topBits16 rfprec (serialise k2)
      LargerThanPage _ _ -> True

    flatten :: Eq k => [Page k] -> [k]
    flatten []            = []
                          -- the min and max key are allowed to be the same
    flatten (p:ps) = case p of
      OneKey k           -> k : flatten ps
      ManyKeys k1 k2     -> k1 : k2 : flatten ps
      LargerThanPage k _ -> k : flatten ps

labelPages :: Pages k -> (Property -> Property)
labelPages (Pages (RFPrecision rfprec) ps0) =
      QC.tabulate "RFPrecision: optimal" [show suggestedRfprec]
    . QC.tabulate "RFPrecision: actual" [show rfprec]
    . QC.tabulate "RFPrecision: |optimal-actual|" [show dist]
    . QC.tabulate "Number of pages" [showPowersOf10 npages]
    . QC.tabulate "Number of 1-key pages" [showPowersOf10 n1]
    . QC.tabulate "Number of many-key pages" [showPowersOf10 n2]
    . QC.tabulate "Number of large-value pages" [showPowersOf10 n3]
  where
    npages = length ps0
    suggestedRfprec = suggestRangeFinderPrecision npages
    dist = abs (suggestedRfprec - rfprec)

    showPowersOf10 :: Int -> String
    showPowersOf10 n0
      | n0 <= 0   = "n == 0"
      | n0 == 1   = "n == 1"
      | otherwise = go n0 1
      where
        go n m | n < m'    = printf "%d < n < %d" m m'
               | otherwise = go n m'
               where m' = 10*m

    (n1,n2,n3) = counts ps0

    counts :: [Page k] -> (Int, Int, Int)
    counts []     = (0, 0, 0)
    counts (p:ps) = let (x, y, z) = counts ps
                    in case p of
                      OneKey{}         -> (x+1, y, z)
                      ManyKeys{}       -> (x, y+1, z)
                      LargerThanPage{} -> (x, y, z+1)

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
