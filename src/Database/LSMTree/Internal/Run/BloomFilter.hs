{-# LANGUAGE TypeApplications #-}

-- | Bloom filters for probing runs during lookups.
--
-- TODO: guarantee file format stability
--
-- === TODO
--
-- This is temporary module header documentation. The module will be
-- fleshed out more as we implement bits of it.
--
-- Related work packages: 5, 6
--
-- This module includes in-memory parts and I\/O parts for, amongst others,
--
-- * Incremental construction
--
-- * Membership queries
--
-- * Calculations for false positive rates, memory use
--
-- * Hashing
--
-- * (de-)serialisation
--
-- The above list is a sketch. Functionality may move around, and the list is
-- not exhaustive.
--
module Database.LSMTree.Internal.Run.BloomFilter (
    -- * Tuning
    -- ** @bloomfilter@ package
    Easy.suggestSizing
  , Easy.safeSuggestSizing
    -- ** Monkey
    -- $tuning
  , monkeyFPR
  , monkeyBits
  , monkeyHashFuncs
    -- * Bloom
  , Bloom
  , Bloom.elem
    -- ** Hashes
  , Hash
  , Hash.Hashable (..)
  , Hash.cheapHashes
    -- ** Incremental construction
  , MBloom
  , Mutable.new
  , newEasy
  , Mutable.insert
  , Bloom.freeze
  , Bloom.unsafeFreeze
  ) where

import           Control.Monad.ST.Strict
import           Data.BloomFilter (Bloom, Hash)
import qualified Data.BloomFilter as Bloom
import qualified Data.BloomFilter.Easy as Easy
import qualified Data.BloomFilter.Hash as Hash
import           Data.BloomFilter.Mutable (MBloom)
import qualified Data.BloomFilter.Mutable as Mutable
import           Prelude hiding (elem)

{-------------------------------------------------------------------------------
  Tuning a la Monkey
-------------------------------------------------------------------------------}

-- $tuning
--
-- These functions are experimental, and will not yet guarantee correct false
-- positive rates. For now, use 'Easy.suggestSizing' and 'Easy.safeSuggestSizing' instead.
--
-- TODO: un-experimental these functions.

-- | Compute the false positive rate for a bloom filter.
--
-- Assumes that the bloom filter uses 'monkeyHashFuncs' hash functions.
--
-- REF: Equation 2 from the paper /Optimal Bloom Filters and Adaptive Merging
-- for LSM-Trees/.
monkeyFPR ::
     Int    -- ^ Number of bits assigned to the bloom filter.
  -> Int    -- ^ Number of entries inserted into the bloom filter.
  -> Double
monkeyFPR numBits numEntries =
    exp ((-(fromIntegral numBits / fromIntegral numEntries)) * (log 2 ** 2))

-- | Compute the number of bits in a bloom filter.
--
-- Assumes that the bloom filter uses 'monkeyHashFuncs' hash functions.
--
-- REF: Equation 2 from the paper /Optimal Bloom Filters and Adaptive Merging
-- for LSM-Trees/, rewritten in terms of @bits@ on page 11.
monkeyBits ::
     Int    -- ^ Number of entries inserted into the bloom filter.
  -> Double -- ^ False positive rate.
  -> Int
monkeyBits numEntries fpr = ceiling $
  (- fromIntegral numEntries) * (log fpr / (log 2 ** 2))

-- | Computes the optimal number of hash functions that minimses the false
-- positive rate for a bloom filter.
--
-- REF: Footnote 2, page 6 from the paper /Optimal Bloom Filters and Adaptive
-- Merging for LSM-Trees/.
monkeyHashFuncs ::
     Int -- ^ Number of bits assigned to the bloom filter.
  -> Int -- ^ Number of entries inserted into the bloom filter.
  -> Int
monkeyHashFuncs numBits numEntries = truncate @Double $
    (fromIntegral numBits / fromIntegral numEntries) * log 2

{-------------------------------------------------------------------------------
  Incremental construction
-------------------------------------------------------------------------------}

-- | Like 'new', but uses 'Easy.suggestSizing' and 'Hash.cheapHashes' by
-- default.
newEasy ::
     Hash.Hashable a
  => Double            -- ^ Requested false-positive rate
  -> Int               -- ^ Number of entries to insert
  -> ST s (MBloom s a)
newEasy targetFpr numEntries = Mutable.new (Hash.cheapHashes numHashFuncs) numBits
  where (numBits, numHashFuncs) = Easy.suggestSizing numEntries targetFpr
