{-# LANGUAGE ScopedTypeVariables #-}

module Database.LSMTree.Generators (
    -- * Range-finder precision
    RFPrecision (..)
  , rfprecInvariant
    -- * Pages (non-partitioned)
  , Pages (..)
  , mkPages
  , pagesInvariant
  , labelPages
    -- * Chunking size
  , ChunkSize (..)
  , chunkSizeInvariant
  ) where

import           Data.Containers.ListUtils (nubOrd)
import           Data.List (sort)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Database.LSMTree.Internal.Run.Index.Compact
                     (rangeFinderPrecisionBounds, suggestRangeFinderPrecision)
import           Database.LSMTree.Internal.Serialise (Serialise (..), topBits16)
import           Test.QuickCheck (Arbitrary (..), NonEmptyList (..), Property,
                     chooseInt, scale, tabulate)
import           Text.Printf (printf)

{-------------------------------------------------------------------------------
  Range-finder precision
-------------------------------------------------------------------------------}

newtype RFPrecision = RFPrecision Int
  deriving Show

instance Arbitrary RFPrecision where
  arbitrary = RFPrecision <$> chooseInt (rfprecLB, rfprecUB)
    where (rfprecLB, rfprecUB) = rangeFinderPrecisionBounds
  shrink (RFPrecision x) =
      [RFPrecision x' | x' <- shrink x , rfprecInvariant (RFPrecision x')]

rfprecInvariant :: RFPrecision -> Bool
rfprecInvariant (RFPrecision x) = x >= rfprecLB && x <= rfprecUB
  where (rfprecLB, rfprecUB) = rangeFinderPrecisionBounds

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
  , getPages                :: [(k, k)]
  }
  deriving Show

instance (Arbitrary k, Ord k, Serialise k) => Arbitrary (Pages k) where
  arbitrary = mkPages <$>
      arbitrary <*> (NonEmpty.fromList . getNonEmpty <$> scale (2*) arbitrary)
  shrink (Pages rfprec ks) = [
        Pages rfprec ks'
      | ks' <- shrink ks, pagesInvariant (Pages rfprec ks')
      ] <> [
        Pages rfprec' ks
      | rfprec' <- shrink rfprec, pagesInvariant (Pages rfprec' ks)
      ] <> [
        Pages rfprec' ks'
      | ks' <- shrink ks
      , rfprec' <- shrink rfprec, pagesInvariant (Pages rfprec' ks')
      ]

mkPages ::
     forall k. (Ord k, Serialise k)
  => RFPrecision
  -> NonEmpty k
  -> Pages k
mkPages rfprec@(RFPrecision n) =
    Pages rfprec . go . nubOrd . sort . NonEmpty.toList
  where
    go :: [k] -> [(k, k)]
    go []          = []
    go [k]         = [(k, k)]
                   -- the min and max key are allowed to be the same
    go  (k1:k2:ks) | topBits16 n (serialise k1) == topBits16 n (serialise k2)
                   = (k1, k2) : go ks
                   | otherwise
                   = (k1, k1) : go (k2 : ks)

pagesInvariant :: (Ord k, Serialise k) => Pages k -> Bool
pagesInvariant (Pages (RFPrecision rfprec) ks) =
       sort ks'   == ks'
    && nubOrd ks' == ks'
    && not (null ks)
    && all partitioned ks
  where
    ks' = flatten ks
    partitioned (kmin, kmax) =
      topBits16 rfprec (serialise kmin) == topBits16 rfprec (serialise kmax)

    flatten :: Eq k => [(k, k)] -> [k]
    flatten []            = []
                          -- the min and max key are allowed to be the same
    flatten ((k1,k2):ks0) | k1 == k2  = k1      : flatten ks0
                          | otherwise = k1 : k2 : flatten ks0

labelPages :: Pages k -> (Property -> Property)
labelPages (Pages (RFPrecision rfprec) ks) =
      tabulate "RFPrecision: optimal" [show suggestedRfprec]
    . tabulate "RFPrecision: actual" [show rfprec]
    . tabulate "RFPrecision: |optimal-actual|" [show dist]
    . tabulate "Number of pages" [showPowersOf10 npages]
  where
    npages = length ks
    suggestedRfprec = suggestRangeFinderPrecision npages
    dist = abs (suggestedRfprec - rfprec)

    showPowersOf10 :: Int -> String
    showPowersOf10 n0
      | n0 <= 0   = error "showPowersOf10"
      | n0 == 1   = "n == 1"
      | otherwise = go n0 1
      where
        go n m | n < m'    = printf "%d < n < %d" m m'
               | otherwise = go n m'
               where m' = 10*m

{-------------------------------------------------------------------------------
  Chunking size
-------------------------------------------------------------------------------}

newtype ChunkSize = ChunkSize Int
  deriving Show

instance Arbitrary ChunkSize where
  arbitrary = ChunkSize <$> chooseInt (chunkSizeLB, chunkSizeUB)
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