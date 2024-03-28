{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Database.LSMTree.Internal.Lookup (
    Run
  , prepLookups
  ) where

import           Data.Vector (Vector)
import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as VU
import           Database.LSMTree.Internal.Run.BloomFilter (Bloom)
import qualified Database.LSMTree.Internal.Run.BloomFilter as Bloom
import           Database.LSMTree.Internal.Run.Index.Compact (CompactIndex,
                     PageSpan, SearchResult)
import qualified Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise

-- | TODO: placeholder type for a run, replace by actual type once implemented
type Run fd = (fd, Bloom SerialisedKey, CompactIndex)

-- | Prepare disk lookups by doing bloom filter queries and index searches.
--
-- Note: results are tagged with a 'KeyIx', an index into the 'SerialisedKey'
-- vector.
prepLookups :: Vector (Run fd) -> Vector SerialisedKey -> Vector (KeyIx, fd, PageSpan)
prepLookups runs ks = V.concatMap f runs
  where
    f r@(fd,_,_) =
        let x = bloomBatch r ks
            y = searchBatch r ks x
        in  V.imapMaybe (\i sres -> (i, fd,) <$> Index.toPageSpan sres) y

type KeyIx = Int

bloomBatch :: Run fd -> Vector SerialisedKey -> Vector Bool
bloomBatch (_fd, b, _ix) = V.map (`Bloom.elem` b)

searchBatch :: Run fd -> Vector SerialisedKey -> Vector Bool -> Vector SearchResult
searchBatch (_fd, _, cix) = V.zipWith f
  where f k b | b         = Index.search k cix
              | otherwise = Index.NoResult
