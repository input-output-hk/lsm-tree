{-# LANGUAGE TupleSections #-}
{- HLINT ignore "Eta reduce" -}

-- | Integration of LSM-Tree components into a full levels structure.
--
-- === TODO
--
-- This is temporary module header documentation. The module will be
-- fleshed out more as we implement bits of it.
--
-- Related work packages: 7
--
-- This module includes in-memory parts and I\/O parts for, amongst others,
--
-- * LSM table handles (multiple runs in multiple levels)
--
-- * Opening and verifying a table
--
-- * Updates (inserts, deletes, mupserts)
--
-- * High performance batch lookups in multiple runs
--
-- * Range lookups
--
-- * Flushing the write buffer when full
--
-- * Merging of runs and levels
--
-- The above list is a sketch. Functionality may move around, and the list is
-- not exhaustive.
--
module Database.LSMTree.Internal.Integration (
    Run
  , prepLookups
  ) where

import           Data.Foldable (Foldable (..))
import           Database.LSMTree.Internal.Run.BloomFilter (Bloom)
import qualified Database.LSMTree.Internal.Run.BloomFilter as Bloom
import           Database.LSMTree.Internal.Run.Index.Compact (CompactIndex,
                     PageSpan)
import qualified Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise

-- | TODO: placeholder type for a run, replace by actual type once implemented
type Run fd = (fd, Bloom SerialisedKey, CompactIndex)

-- | Prepare disk lookups by doing bloom filter queries and index searches.
--
-- Note: results are grouped by key instead of file descriptor, because this
-- means that results for a single key are close together.
prepLookups :: [Run fd] -> [SerialisedKey] -> [(SerialisedKey, (fd, PageSpan))]
prepLookups runs ks =
    [ (k, (fd, pspan))
    | k <- ks
    , r@(fd,_,_) <- runs
    , pspan <- toList (prepLookup r k)
    ]

prepLookup :: Run fd -> SerialisedKey -> Maybe PageSpan
prepLookup (_fd, b, fpix) k
  | Bloom.elem k b = Index.toPageSpan $ Index.search k fpix
  | otherwise      = Nothing
