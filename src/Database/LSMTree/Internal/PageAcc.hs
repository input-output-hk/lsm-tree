{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Page accumulator.
module Database.LSMTree.Internal.PageAcc (
    MPageAcc,
    newPageAcc,
    resetPageAcc,
    pageAccAddElem,
    serializePageAcc,
) where

import           Control.Monad.ST.Strict (ST)
import           Data.Bits (unsafeShiftL, (.|.))
import qualified Data.Primitive as P
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Primitive as PV
import           Data.Word (Word16, Word32, Word64)
import           Database.LSMTree.Internal.BitMath
import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import           Database.LSMTree.Internal.Entry (Entry (..))
import           Database.LSMTree.Internal.RawPage
import           Database.LSMTree.Internal.Serialise
import           Database.LSMTree.Internal.Serialise.RawBytes (RawBytes (..))
import qualified Database.LSMTree.Internal.Serialise.RawBytes as RB

-- |
--
-- The delete operations take the least amount of space, as there's only the key.
--
-- A smallest page is with empty key:
--
-- >>> import FormatPage
-- >>> let Just page0 = pageSizeAddElem (Key "", Delete, Nothing) pageSizeEmpty
-- >>> page0
-- page0
--
-- Then we can add pages with a single byte key, e.g.
--
-- >>> pageSizeAddElem (Key "a", Delete, Nothing) page0
-- Just (PageSize {pageSizeElems = 2, pageSizeBlobs = 0, pageSizeBytes = 35})
--
-- i.e. roughly 3-4 bytes (when we get to 32/64 elements we add more bytes for bitmaps).
-- (key and value offset is together 4 bytes: so it's at least 4, the encoding of single element page takes more space).
--
-- If we write as small program, adding single byte keys to a page size:
--
-- >>> let calc s ps = case pageSizeAddElem (Key "x", Delete, Nothing) ps of { Nothing -> s; Just ps' -> calc (s + 1) ps' }
-- >>> calc 1 page0
-- 759
--
-- I.e. we can have a 4096 byte page with at most 759 keys, actually less,
-- as there are only 256 single byte keys.
--
-- >>> let calc2 s ps = case pageSizeAddElem (Key $ if s < 257 then "x" else "xx", Delete, Nothing) ps of { Nothing -> s; Just ps' -> calc2 (s + 1) ps' }
-- >>> calc2 1 page0
-- 680
--
-- Rounded to the next multiple of 64 is 704
--
-- 'MPageAcc' can hold up to 704 elements, but most likely 'pageAccAddElem' will make it overflow sooner.
-- Having an upper bound allows us to allocate all memory for the accumulator in advance.
--
-- We don't store or calculate individual key nor value offsets in 'MPageAcc', as these will be naturally calculated during serialization ('serializePageAcc').
--
data MPageAcc s = MPageAcc
    { mpDir         :: !(P.MutablePrimArray s Int)      -- ^ various counters (directory + extra counters). It is convenient to have counters as 'Int', as all indexing uses 'Int's.
    , mpOpMap       :: !(P.MutablePrimArray s Word64)   -- ^ operations crumb map
    , mpBlobRefsMap :: !(P.MutablePrimArray s Word64)   -- ^ blob reference bitmap
    , mpBlobRefs1   :: !(P.MutablePrimArray s Word64)   -- ^ blob spans 64 bit part (offset)
    , mpBlobRefs2   :: !(P.MutablePrimArray s Word32)   -- ^ blob spans 32 bit part (size)
    , mpKeys        :: !(V.MVector s SerialisedKey)     -- ^ keys
    , mpValues      :: !(V.MVector s SerialisedValue)   -- ^ values
    }

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

keysCountIdx :: Int
keysCountIdx = 0
{-# INLINE keysCountIdx #-}

blobRefCountIdx :: Int
blobRefCountIdx = 1
{-# INLINE blobRefCountIdx #-}

byteSizeIdx :: Int
byteSizeIdx = 2
{-# INLINE byteSizeIdx #-}

keysSizeIdx :: Int
keysSizeIdx = 3
{-# INLINE keysSizeIdx #-}

pageSize :: Int
pageSize = 4096
{-# INLINE pageSize #-}

-------------------------------------------------------------------------------
-- Entry operations
-------------------------------------------------------------------------------

-- | Calculate the total byte size of key, value and optional blobspan.
--
-- To fit into single page this has to be at most 4052 (the same as the max key size),
-- if the entry is larger, the 'pageAccAddElem' would immediately return 'Nothing'.
--
-- In other words, if you have a large entry (i.e. Insert with big value), don't use the 'MPageAcc', but construct the single value page directly.
--
-- Checking entry size allows us to use 'Word16' arithmetic, we don't need to worry about overflows.
sizeofEntry :: SerialisedKey -> Entry SerialisedValue BlobSpan -> Int
sizeofEntry k Delete               = sizeofKey k
sizeofEntry k (Mupdate v)          = sizeofKey k + sizeofValue v
sizeofEntry k (Insert v)           = sizeofKey k + sizeofValue v
sizeofEntry k (InsertWithBlob v _) = sizeofKey k + sizeofValue v + 12

-- | Whether 'Entry' adds a blob reference
hasBlobRef :: Entry a b -> Bool
hasBlobRef (InsertWithBlob _ _) = True
hasBlobRef _                    = False

-- | Entry's operations crumb. We return 'Word64' as we'd write that.
entryCrumb :: Entry SerialisedValue BlobSpan -> Word64
entryCrumb Insert {}         = 0
entryCrumb InsertWithBlob {} = 0
entryCrumb Mupdate {}        = 1
entryCrumb Delete {}         = 2

-- | Entry value. Return 'emptyValue' for 'Delete'
-- (the empty value is in the page even for 'Delete's)
entryValue :: Entry SerialisedValue BlobSpan -> SerialisedValue
entryValue (Insert v)           = v
entryValue (InsertWithBlob v _) = v
entryValue (Mupdate v)          = v
entryValue Delete               = emptyValue

emptyValue :: SerialisedValue
emptyValue = SerialisedValue (RB.pack [])

-------------------------------------------------------------------------------
-- PageAcc functions
-------------------------------------------------------------------------------

-- | Create new 'MPageAcc'.
--
-- The create 'MPageAcc' will be empty.
newPageAcc :: ST s (MPageAcc s)
newPageAcc = do
    mpDir          <- P.newPrimArray 4
    mpOpMap        <- P.newPrimArray 11 -- 704 / 64
    mpBlobRefsMap  <- P.newPrimArray 22 -- 704 / 32
    mpBlobRefs1    <- P.newPrimArray 680
    mpBlobRefs2    <- P.newPrimArray 680
    mpKeys         <- MV.new 680
    mpValues       <- MV.new 680

    -- reset the memory, as it's not initialized
    let page = MPageAcc {..}
    resetPageAcc page
    return page

-- | Reuse 'MPageAcc' for constructing new page, the old data will be reset.
resetPageAcc :: MPageAcc s
    -> ST s ()
resetPageAcc MPageAcc {..} = do
    P.setPrimArray mpDir 0 4 0
    P.setPrimArray mpOpMap 0 11 0
    P.setPrimArray mpBlobRefsMap 0 22 0

    -- we don't need to clear these, we set what we need.
    -- P.setPrimArray mpBlobRefs1 0 680 0
    -- P.setPrimArray mpBlobRefs1 0 680 0

    -- initial size is 8 bytes for directory and 2 bytes for last value offset.
    P.writePrimArray mpDir byteSizeIdx 10

-- | Add an entry to 'MPageAcc'.
--
pageAccAddElem
    :: MPageAcc s
    -> SerialisedKey
    -> Entry SerialisedValue BlobSpan
    -> ST s Bool   -- ^ 'True' if value was successfully added.
pageAccAddElem MPageAcc {..} k e
    -- quick short circuit: if the entry is bigger than page: no luck.
    | sizeofEntry k e >= pageSize = return False
    | otherwise = do
        n <- P.readPrimArray mpDir keysCountIdx
        b <- P.readPrimArray mpDir blobRefCountIdx
        s <- P.readPrimArray mpDir byteSizeIdx

        let !n' = n + 1
        let !b' = if hasBlobRef e then b + 1 else b

        let !s' = s
               + (if mod64 n == 0 then 8 else 0)         -- blobrefs bitmap
               + (if mod32 n == 0 then 8 else 0)         -- operations bitmap
               + (case n of { 0 -> 6; 1 -> 2; _ -> 4 })  -- key and value offsets
               + sizeofEntry k e

        -- check for size overflow
        if s' > pageSize
        then return False
        else do
            -- key sizes
            ks <- P.readPrimArray mpDir keysSizeIdx
            let !ks' = ks + sizeofKey k

            P.writePrimArray mpDir keysCountIdx n'
            P.writePrimArray mpDir blobRefCountIdx b'
            P.writePrimArray mpDir keysSizeIdx ks'
            P.writePrimArray mpDir byteSizeIdx s'

            -- blob reference
            case e of
                InsertWithBlob _ (BlobSpan w64 w32) -> do
                    setBlobRef mpBlobRefsMap n
                    P.writePrimArray mpBlobRefs1 b w64
                    P.writePrimArray mpBlobRefs2 b w32
                _ -> return ()

            -- operation
            setOperation mpOpMap n (entryCrumb e)

            -- keys and values
            MV.write mpKeys n k
            MV.write mpValues n (entryValue e)

            return True

setBlobRef :: P.MutablePrimArray s Word64 -> Int -> ST s ()
setBlobRef arr i = do
    let !j = div64 i
    let !k = mod64 i
    w64 <- P.readPrimArray arr j
    let w64' = w64 .|. unsafeShiftL 1 k
    P.writePrimArray arr j $! w64'

setOperation :: P.MutablePrimArray s Word64 -> Int -> Word64 -> ST s ()
setOperation arr i crumb = do
    let !j = div32 i
    let !k = mod32 i
    w64 <- P.readPrimArray arr j
    let w64' = w64 .|. unsafeShiftL crumb (mul2 k)
    P.writePrimArray arr j $! w64'

-- | Convert mutable 'MPageAcc' accumulator to concrete 'RawPage'.
--
-- After this operation 'MPageAcc' argument can be reset with 'resetPageAcc',
-- and reused.
serializePageAcc :: MPageAcc s -> ST s RawPage
serializePageAcc page@MPageAcc {..} = do
    size <- P.readPrimArray mpDir keysCountIdx
    case size of
        0 -> return emptyRawPage
        _ -> serializePageAccN size page

-- | Serialize non-empty page.
serializePageAccN :: forall s. Int -> MPageAcc s -> ST s RawPage
serializePageAccN size MPageAcc {..} = do
    b  <- P.readPrimArray mpDir blobRefCountIdx
    ks <- P.readPrimArray mpDir keysSizeIdx

    -- keys offsets offset
    let ko0 :: Int
        !ko0 = 8
             + mul8 (ceilDiv64 size + ceilDiv64 (mul2 size)) -- blob and operations bitmaps
             + mul8 b + mul4 b                               -- blob refs

    -- values offset offset
    let vo0 :: Int
        !vo0 = ko0 + mul2 size

    -- keys data offset
    let kd0 :: Int
        !kd0 = vo0 + if size == 1 then 6 else mul2 (size + 1)

    -- values data offset
    let vd0 :: Int
        !vd0 = kd0 + ks

    -- allocate bytearray
    ba <- P.newByteArray pageSize :: ST s (P.MutableByteArray s)
    P.fillByteArray ba 0 pageSize 0

    -- directory: 64 bytes
    P.writeByteArray ba 0 (fromIntegral size :: Word16)
    P.writeByteArray ba 1 (fromIntegral b :: Word16)
    P.writeByteArray ba 2 (fromIntegral ko0 :: Word16)

    -- blobref and operation bitmap sizes in bytes
    let !blobRefMapSize = mul8 (ceilDiv64 size)
    let !opMapSize      = mul8 (ceilDiv64 (mul2 size))

    -- traceM $ "sizes " ++ show (blobRefMapSize, opMapSize)

    P.copyMutableByteArray ba 8                    (primToByte mpBlobRefsMap) 0 blobRefMapSize
    P.copyMutableByteArray ba (8 + blobRefMapSize) (primToByte mpOpMap)       0 opMapSize

    -- blob references
    P.copyMutableByteArray ba (8 + blobRefMapSize + opMapSize)          (primToByte mpBlobRefs1) 0 (mul8 b)
    P.copyMutableByteArray ba (8 + blobRefMapSize + opMapSize + mul8 b) (primToByte mpBlobRefs2) 0 (mul4 b)

    -- traceM $ "preloop " ++ show (ko0, kd0, vd0)

    let loop :: Int -> Int -> Int -> Int -> Int -> ST s ()
        loop !i !ko !vo !kd !vd
            | i >= size = do
                  -- past last value offset.
                  -- Note: we don't need to write this offset as Word32
                  -- even in size == 1 case, as preconditions require the
                  -- entries to fit into page, i.e. fit into Word16.
                  P.writeByteArray ba (div2 vo) (fromIntegral vd :: Word16)

            | otherwise = do
                  -- traceM $ "loop " ++ show (i, ko, kd, vo, vd)

                  k <- MV.read mpKeys i
                  v <- MV.read mpValues i
                  let SerialisedKey   (RawBytes (PV.Vector koff klen kba)) = k
                      SerialisedValue (RawBytes (PV.Vector voff vlen vba)) = v

                  -- key and value offsets
                  P.writeByteArray ba (div2 ko) (fromIntegral kd :: Word16)
                  P.writeByteArray ba (div2 vo) (fromIntegral vd :: Word16)

                  -- key and value data
                  P.copyByteArray ba kd kba koff klen
                  P.copyByteArray ba vd vba voff vlen

                  loop (i + 1) (ko + 2) (vo + 2) (kd + klen) (vd + vlen)

    loop 0 ko0 vo0 kd0 vd0

    ba' <- P.unsafeFreezeByteArray ba
    return (makeRawPage ba' 0)

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

-- | Extract underlying bytearray fromn 'P.MutableByteArray',
-- so we can copy its contents.
primToByte :: P.MutablePrimArray s a -> P.MutableByteArray s
primToByte (P.MutablePrimArray ba) = P.MutableByteArray ba
