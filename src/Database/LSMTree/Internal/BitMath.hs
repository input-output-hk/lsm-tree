-- | Some arithmetic implemented using bit operations
--
-- Valid for non-negative arguments.
module Database.LSMTree.Internal.BitMath (
    -- * Two
    div2,
    mod2,
    mul2,
    ceilDiv2,
    -- * Four
    div4,
    mod4,
    mul4,
    -- * Eight
    div8,
    mod8,
    mul8,
    ceilDiv8,
    -- * Sixteen
    div16,
    mod16,
    mul16,
    -- * 32
    div32,
    mod32,
    -- * 64
    div64,
    mod64,
    mul64,
    ceilDiv64,
    -- * 4096, page size
    roundUpToPageSize,
) where

import           Data.Bits

-------------------------------------------------------------------------------
-- 2
-------------------------------------------------------------------------------

div2 :: Bits a => a -> a
div2 x = unsafeShiftR x 1
{-# INLINE div2 #-}

mod2 :: (Bits a, Num a) => a -> a
mod2 x = x .&. 1
{-# INLINE mod2 #-}

mul2 :: Bits a => a -> a
mul2 x = unsafeShiftL x 1
{-# INLINE mul2 #-}

ceilDiv2 :: (Bits a, Num a) => a -> a
ceilDiv2 i = unsafeShiftR (i + 1) 1
{-# INLINE ceilDiv2 #-}

-------------------------------------------------------------------------------
-- 4
-------------------------------------------------------------------------------

div4 :: Bits a => a -> a
div4 x = unsafeShiftR x 2
{-# INLINE div4 #-}

mod4 :: (Bits a, Num a) => a -> a
mod4 x = x .&. 3
{-# INLINE mod4 #-}

mul4 :: Bits a => a -> a
mul4 x = unsafeShiftL x 2
{-# INLINE mul4 #-}

-------------------------------------------------------------------------------
-- 8
-------------------------------------------------------------------------------

div8 :: Bits a => a -> a
div8 x = unsafeShiftR x 3
{-# INLINE div8 #-}

mod8 :: (Bits a, Num a) => a -> a
mod8 x = x .&. 7
{-# INLINE mod8 #-}

mul8 :: Bits a => a -> a
mul8 x = unsafeShiftL x 3
{-# INLINE mul8 #-}

ceilDiv8 :: (Bits a, Num a) => a -> a
ceilDiv8 i = unsafeShiftR (i + 7) 3
{-# INLINE ceilDiv8 #-}

-------------------------------------------------------------------------------
-- 16
-------------------------------------------------------------------------------

div16 :: Bits a => a -> a
div16 x = unsafeShiftR x 4
{-# INLINE div16 #-}

mod16 :: (Bits a, Num a) => a -> a
mod16 x = x .&. 15
{-# INLINE mod16 #-}

mul16 :: Bits a => a -> a
mul16 x = unsafeShiftL x 4
{-# INLINE mul16 #-}

-------------------------------------------------------------------------------
-- 32
-------------------------------------------------------------------------------

div32 :: Bits a => a -> a
div32 x = unsafeShiftR x 5
{-# INLINE div32 #-}

mod32 :: (Bits a, Num a) => a -> a
mod32 x = x .&. 31
{-# INLINE mod32 #-}

-------------------------------------------------------------------------------
-- 64
-------------------------------------------------------------------------------

-- |
--
-- >>> map div64 [0 :: Word, 1, 63, 64, 65]
-- [0,0,0,1,1]
--
div64 :: Bits a => a -> a
div64 x = unsafeShiftR x 6
{-# INLINE div64 #-}

-- |
--
-- >>> map mod64 [0 :: Word, 1, 63, 64, 65]
-- [0,1,63,0,1]
--
mod64 :: (Bits a, Num a) => a -> a
mod64 x = x .&. 63
{-# INLINE mod64 #-}

mul64 :: Bits a => a -> a
mul64 x = unsafeShiftL x 6
{-# INLINE mul64 #-}

-- | rounding up division
--
-- >>> map ceilDiv64 [0 :: Word, 1, 63, 64, 65]
-- [0,1,1,1,2]
--
ceilDiv64 :: (Bits a, Num a) => a -> a
ceilDiv64 i = unsafeShiftR (i + 63) 6
{-# INLINE ceilDiv64 #-}

-------------------------------------------------------------------------------
-- 4096
-------------------------------------------------------------------------------

roundUpToPageSize :: Int -> Int
roundUpToPageSize n =
    -- assumes pageSize = 4096:
    ((n + 0x0fff) .&. complement 0x0fff)
