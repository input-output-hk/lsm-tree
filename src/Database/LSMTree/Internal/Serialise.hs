{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE UnboxedTuples              #-}

#include <MachDeps.h>

-- | Temporary placeholders for serialisation.
module Database.LSMTree.Internal.Serialise (
    Serialise (..)
  , SerialisedKey (..)
  , topBits16
  , sliceBits32
  ) where

import           Data.Bits (Bits (shiftL, shiftR))
import qualified Data.ByteString.Short as SBS
import           Data.ByteString.Short.Internal (ShortByteString (SBS))
import           Data.Kind (Type)
import           Data.Primitive.ByteArray (ByteArray (..))
import           Database.LSMTree.Internal.Run.BloomFilter (Hashable (..))
import           GHC.Exts
import           GHC.Word

-- | Serialisation into a 'SerialisedKey' (i.e., a 'ByteArray').
--
-- Comparison of @a@-values should be preserved by serialisation. Note that
-- 'SerialisedKey's are lexicographically ordered.
--
-- INVARIANT: Serialisation should preserve ordering, @x `compare` y ==
-- serialise x `compare` serialise y@. Serialised keys are lexicographically
-- ordered, in particular this means that values should be serialised into
-- big-endian formats.
class Serialise a where
  serialise :: a -> SerialisedKey

-- | A first attempt at a representation for a serialised key.
--
-- Serialisation should preserve equality and ordering. The 'Ord' instance for
-- 'SerialisedKey' uses lexicographical ordering.
type SerialisedKey :: Type
newtype SerialisedKey = SerialisedKey ByteArray
  deriving newtype (Show, Eq)

-- | Re-use lexicographical 'Ord' instance from 'ShortByteString'.
instance Ord SerialisedKey where
  (SerialisedKey (ByteArray skey1#)) `compare` (SerialisedKey (ByteArray skey2#)) =
      SBS skey1# `compare` SBS skey2#

-- TODO: optimisation
instance Hashable SerialisedKey where
  hashIO32 :: SerialisedKey -> Word32 -> IO Word32
  hashIO32 (SerialisedKey (ByteArray ba#)) =
    hashIO32 (SBS.fromShort $ SBS ba#)

-- | @'topBits16' n k@ slices the first @n@ bits from the /top/ of the
-- serialised key @k@. Returns the string of bits as a 'Word16'.
--
-- The /top/ corresponds to the most significant bit (big-endian).
--
-- PRECONDITION: @n >= 0 && n <= 16. We can slice out at most 16 bits,
-- all bits beyond that are truncated.
--
-- PRECONDITION: The byte-size of the serialised key should be at least 2 bytes.
--
-- TODO: optimisation ideas: use unsafe shift/byteswap primops, look at GHC
-- core, find other opportunities for using primops.
--
topBits16 :: Int -> SerialisedKey -> Word16
topBits16 n (SerialisedKey (ByteArray k#)) = shiftR w16 (16 - n)
  where
    w16 = toWord16 (indexWord8ArrayAsWord16# k# 0#)

toWord16 :: Word16# -> Word16
#if WORDS_BIGENDIAN
toWord16 = W16#
#else
toWord16 x# = byteSwap16 (W16# x#)
#endif

-- | @'sliceBits32' off k@ slices from the serialised key @k@ a string of @32@
-- bits, starting at the @0@-based offset @off@. Returns the string of bits as a
-- 'Word32'.
--
-- Offsets are counted in bits from the /top/: offset @0@ corresponds to the
-- most significant bit (big-endian).
--
-- PRECONDITION: the byte-size @n@ of the serialised key should be at least 4
-- bytes.
--
-- PRECONDITION: The serialised key should be large enough that we can slice out
-- 4 bytes after of the bit-offset @off, since we can only slice out bits that
-- are within the bounds of the byte array.
--
-- TODO: optimisation ideas: use unsafe shift/byteswap primops, look at GHC
-- core, find other opportunities for using primops.
--
sliceBits32 :: Int -> SerialisedKey -> Word32
sliceBits32 (I# off#) (SerialisedKey (ByteArray k#))
    | 0# <- r#
    =   toWord32 (indexWord8ArrayAsWord32# k# q#)
    | otherwise
    =   toWord32 (indexWord8ArrayAsWord32# k# q#        ) `shiftL` r
      + w8w32#   (indexWord8Array#         k# (q# +# 4#)) `shiftR` (8 - r)
  where
    !(# q#, r# #) = quotRemInt# off# 8#
    r             = I# r#
    -- No need for byteswapping here
    w8w32# x#     = W32# (wordToWord32# (word8ToWord# x#))

toWord32 :: Word32# -> Word32
#if WORDS_BIGENDIAN
toWord32 = W32#
#else
toWord32 x# = byteSwap32 (W32# x#)
#endif
