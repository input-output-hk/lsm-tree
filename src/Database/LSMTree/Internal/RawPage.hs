{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Database.LSMTree.Internal.RawPage (
    RawPage,
    emptyRawPage,
    makeRawPage,
    unsafeMakeRawPage,
    rawPageRawBytes,
    rawPageNumKeys,
    rawPageNumBlobs,
    rawPageLookup,
    RawPageLookup(..),
    rawPageOverflowPages,
    rawPageIndex,
    RawPageIndex(..),
    -- * Debug
    rawPageKeyOffsets,
    rawPageValueOffsets,
    rawPageValueOffsets1,
    rawPageHasBlobSpanAt,
    rawPageBlobSpanIndex,
    rawPageOpAt,
    rawPageKeys,
    rawPageValues,
) where

import           Control.DeepSeq (NFData (rnf))
import           Control.Exception (assert)
import           Data.Bits (complement, popCount, unsafeShiftL, unsafeShiftR,
                     (.&.))
import           Data.Primitive.ByteArray (ByteArray (..), byteArrayFromList,
                     copyByteArray, fillByteArray, indexByteArray,
                     isByteArrayPinned, newAlignedPinnedByteArray, runByteArray,
                     sizeofByteArray)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as PV
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.LSMTree.Internal.BitMath
import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import           Database.LSMTree.Internal.Entry (Entry (..))
import           Database.LSMTree.Internal.RawBytes (RawBytes (..))
import qualified Database.LSMTree.Internal.RawBytes as RB
import           Database.LSMTree.Internal.Serialise (SerialisedKey (..),
                     SerialisedValue (..))
import           Database.LSMTree.Internal.Vector
import           GHC.List (foldl')

-------------------------------------------------------------------------------
-- RawPage type
-------------------------------------------------------------------------------

data RawPage = RawPage
    !Int        -- ^ offset in Word16s.
    !ByteArray
  deriving (Show)

emptyRawPage :: RawPage
emptyRawPage = flip makeRawPage 0 $ byteArrayFromList
   [ 0, 0, 0, 0
   , 8, 0, 0, 0
   , 10 :: Word8
   ]

invariant :: RawPage -> Bool
invariant (RawPage off ba) = and
    [ off >= 0                                      -- offset is positive
    , mod8 offsetInBytes == 0                       -- 64 bit/8 byte alignment
    , (sizeofByteArray ba - offsetInBytes) >= 4096  -- there is always 4096 bytes
    , isByteArrayPinned ba                          -- bytearray should be pinned
    ]
  where
    offsetInBytes = mul2 off

instance NFData RawPage where
  rnf (RawPage _ _) = ()

-- | This instance assumes pages are 4096 bytes in size
instance Eq RawPage where
    RawPage off1 ba1 == RawPage off2 ba2 = v1 == v2
      where
        v1, v2 :: PV.Vector Word16
        v1 = mkPrimVector off1 2048 ba1
        v2 = mkPrimVector off2 2048 ba2

-- | Create 'RawPage'.
--
-- This function may copy data to satisfy internal 'RawPage' invariants.
-- Use 'unsafeMakeRawPage' if you don't want copy.
makeRawPage
    :: ByteArray  -- ^ bytearray, must contain 4096 bytes (after offset)
    -> Int        -- ^ offset in bytes, must be 8 byte aligned.
    -> RawPage
makeRawPage ba off
    | invariant page = page
    | otherwise      = RawPage 0 $ runByteArray $ do
        mba <- newAlignedPinnedByteArray 4096 8
        fillByteArray mba 0 4096 0
        copyByteArray mba 0 ba off (clamp 0 4096 (sizeofByteArray ba - off))
        return mba
  where
    page = RawPage (div2 off) ba
    clamp l u x = max l (min u x)

unsafeMakeRawPage
    :: ByteArray  -- ^ bytearray, must be pinned and contain 4096 bytes (after offset)
    -> Int        -- ^ offset in bytes, must be 8 byte aligned.
    -> RawPage
unsafeMakeRawPage ba off = assert (invariant page) page
  where
    page = RawPage (div2 off) ba

rawPageRawBytes :: RawPage -> RawBytes
rawPageRawBytes (RawPage off ba) =
      RB.fromByteArray (mul2 off) 4096 ba

-------------------------------------------------------------------------------
-- Lookup function
-------------------------------------------------------------------------------

data RawPageLookup entry =
       -- | The key is not present on the page.
       LookupEntryNotPresent

       -- | The key is present and corresponds to a normal entry that fits
       -- fully within the page.
     | LookupEntry !entry

       -- | The key is present and corresponds to an entry where the value
       -- may have overflowed onto subsequent pages. In this case the entry
       -- contains the /prefix/ of the value (that did fit on the page). The
       -- length of the suffix is returned separately.
     | LookupEntryOverflow !entry !Word32
  deriving (Eq, Functor, Show)

rawPageLookup
    :: RawPage
    -> SerialisedKey
    -> RawPageLookup (Entry SerialisedValue BlobSpan)
rawPageLookup !page !key
  | dirNumKeys == 1 = lookup1
  | otherwise       = bisect 0 (fromIntegral dirNumKeys)
  where
    !dirNumKeys = rawPageNumKeys page

    lookup1
      | key == rawPageKeyAt page 0
      = let !entry  = rawPageEntry1 page
            !suffix = rawPageSingleValueSuffix page
         in if suffix > 0
              then LookupEntryOverflow entry suffix
              else LookupEntry         entry
      | otherwise
      = LookupEntryNotPresent

    -- when to switch to linear scan
    -- this a tuning knob
    -- can be set to zero.
    threshold = 3

    bisect :: Int -> Int -> RawPageLookup (Entry SerialisedValue BlobSpan)
    bisect !i !j
        | j - i < threshold = linear i j
        | otherwise = case compare key (rawPageKeyAt page k) of
            EQ -> LookupEntry (rawPageEntryAt page k)
            GT -> bisect (k + 1) j
            LT -> bisect i k
      where
        k = i + div2 (j - i)

    linear :: Int -> Int -> RawPageLookup (Entry SerialisedValue BlobSpan)
    linear !i !j
        | i >= j                     = LookupEntryNotPresent
        | key == rawPageKeyAt page i = LookupEntry (rawPageEntryAt page i)
        | otherwise                  = linear (i + 1) j

data RawPageIndex entry =
       IndexNotPresent
       -- | The index is present and corresponds to a normal entry that fits
       -- fully within the page (but might be the only entry on the page).
     | IndexEntry !SerialisedKey !entry
       -- | The index is present and corresponds to an entry where the value
       -- has overflowed onto subsequent pages. In this case only prefix entry
       -- and the length of the suffix are returned.
       -- The caller can copy the full serialised pages themselves.
     | IndexEntryOverflow !SerialisedKey !entry !Word32
  deriving (Eq, Functor, Show)

{-# INLINE rawPageIndex #-}
rawPageIndex
    :: RawPage
    -> Word16
    -> RawPageIndex (Entry SerialisedValue BlobSpan)
rawPageIndex !page !ix
  | ix >= dirNumKeys =
      IndexNotPresent
  | dirNumKeys > 1 =
      let ix' = fromIntegral ix
       in IndexEntry (rawPageKeyAt page ix') (rawPageEntryAt page ix')
  | otherwise =
      let key = rawPageKeyAt page 0
          entry = rawPageEntry1 page
          !suffix = rawPageSingleValueSuffix page
       in if suffix <= 0
         then IndexEntry key entry
         else IndexEntryOverflow key entry suffix
  where
    !dirNumKeys = rawPageNumKeys page

-- | for non-single key page case
rawPageEntryAt :: RawPage -> Int -> Entry SerialisedValue BlobSpan
rawPageEntryAt page i =
    case rawPageOpAt page i of
      0 -> if rawPageHasBlobSpanAt page i == 0
           then Insert (rawPageValueAt page i)
           else InsertWithBlob (rawPageValueAt page i)
                               (rawPageBlobSpanIndex page
                                  (rawPageCalculateBlobIndex page i))
      1 -> Mupdate (rawPageValueAt page i)
      _ -> Delete

-- | single key page case
rawPageEntry1 :: RawPage -> Entry SerialisedValue BlobSpan
rawPageEntry1 page =
    case rawPageOpAt page 0 of
      0 -> if rawPageHasBlobSpanAt page 0 == 0
           then Insert (rawPageSingleValuePrefix page)
           else InsertWithBlob (rawPageSingleValuePrefix page)
                               (rawPageBlobSpanIndex page
                                  (rawPageCalculateBlobIndex page 0))
      1 -> Mupdate (rawPageSingleValuePrefix page)
      _ -> Delete

-- | Calculate the number of overflow pages that are expected to follow this
-- page.
--
-- This will be non-zero when the page contains a single key\/op entry that is
-- itself too large to fit within the page.
--
rawPageOverflowPages :: RawPage -> Int
rawPageOverflowPages page
  | rawPageNumKeys page == 1
  , let (_, end) = rawPageValueOffsets1 page
  = fromIntegral (ceilDivPageSize end - 1)  -- don't count the first page

  | otherwise = 0

-------------------------------------------------------------------------------
-- Accessors
-------------------------------------------------------------------------------

-- Note: indexByteArray's offset is given in elements of type a rather than in bytes.

rawPageNumKeys :: RawPage -> Word16
rawPageNumKeys (RawPage off ba) = indexByteArray ba off

rawPageNumBlobs :: RawPage -> Word16
rawPageNumBlobs (RawPage off ba) = indexByteArray ba (off + 1)

rawPageKeysOffset :: RawPage -> Word16
rawPageKeysOffset (RawPage off ba) = indexByteArray ba (off + 2)

type KeyOffset = Word16
type ValueOffset = Word16

rawPageKeyOffsets :: RawPage -> PV.Vector KeyOffset
rawPageKeyOffsets page@(RawPage off ba) =
    mkPrimVector
        (off + fromIntegral (div2 dirOffset))
        (fromIntegral dirNumKeys + 1)
        ba
  where
    !dirNumKeys = rawPageNumKeys page
    !dirOffset  = rawPageKeysOffset page

-- | for non-single key page case
rawPageValueOffsets :: RawPage -> PV.Vector ValueOffset
rawPageValueOffsets page@(RawPage off ba) =
    assert (dirNumKeys /= 1) $
    mkPrimVector
        (off + fromIntegral (div2 dirOffset) + fromIntegral dirNumKeys)
        (fromIntegral dirNumKeys + 1)
        ba
  where
    !dirNumKeys = rawPageNumKeys page
    !dirOffset  = rawPageKeysOffset page

-- | single key page case
{-# INLINE rawPageValueOffsets1 #-}
rawPageValueOffsets1 :: RawPage -> (Word16, Word32)
rawPageValueOffsets1 page@(RawPage off ba) =
    assert (rawPageNumKeys page == 1) $
    ( indexByteArray ba (off + fromIntegral (div2 dirOffset) + 1)
    , indexByteArray ba (div2 (off + fromIntegral (div2 dirOffset)) + 1)
    )
  where
    !dirOffset = rawPageKeysOffset page

rawPageHasBlobSpanAt :: RawPage -> Int -> Word64
rawPageHasBlobSpanAt _page@(RawPage off ba) i = do
    let j = div64 i
    let k = mod64 i
    let word = indexByteArray ba (div4 off + 1 + j)
    unsafeShiftR word k .&. 1

rawPageOpAt :: RawPage -> Int -> Word64
rawPageOpAt page@(RawPage off ba) i = do
    let j = div32 i
    let k = mod32 i
    let word = indexByteArray ba (div4 off + 1 + ceilDiv64 (fromIntegral dirNumKeys) + j)
    unsafeShiftR word (mul2 k) .&. 3
  where
    !dirNumKeys = rawPageNumKeys page

rawPageKeys :: RawPage -> V.Vector SerialisedKey
rawPageKeys page@(RawPage off ba) = do
    let offs = rawPageKeyOffsets page
    V.fromList
        [ SerialisedKey (RB.fromByteArray (mul2 off + start) (end - start) ba)
        | i <- [ 0 .. fromIntegral dirNumKeys -  1 ] :: [Int]
        , let start = fromIntegral (PV.unsafeIndex offs i) :: Int
        , let end   = fromIntegral (PV.unsafeIndex offs (i + 1)) :: Int
        ]
  where
    !dirNumKeys = rawPageNumKeys page

rawPageKeyAt :: RawPage -> Int -> SerialisedKey
rawPageKeyAt page@(RawPage off ba) i = do
    SerialisedKey (RB.fromByteArray (mul2 off + start) (end - start) ba)
  where
    offs  = rawPageKeyOffsets page
    start = fromIntegral (PV.unsafeIndex offs i) :: Int
    end   = fromIntegral (PV.unsafeIndex offs (i + 1)) :: Int

-- | Non-single page case
rawPageValues :: RawPage -> V.Vector SerialisedValue
rawPageValues page@(RawPage off ba) =
    let offs = rawPageValueOffsets page in
    V.fromList
        [ SerialisedValue $ RB.fromByteArray (mul2 off + start) (end - start) ba
        | i <- [ 0 .. fromIntegral dirNumKeys -  1 ] :: [Int]
        , let start = fromIntegral (PV.unsafeIndex offs i) :: Int
        , let end   = fromIntegral (PV.unsafeIndex offs (i + 1)) :: Int
        ]
  where
    !dirNumKeys = rawPageNumKeys page

rawPageValueAt :: RawPage -> Int -> SerialisedValue
rawPageValueAt page@(RawPage off ba) i =
    SerialisedValue (RB.fromByteArray (mul2 off + start) (end - start) ba)
  where
    offs  = rawPageValueOffsets page
    start = fromIntegral (PV.unsafeIndex offs i) :: Int
    end   = fromIntegral (PV.unsafeIndex offs (i + 1)) :: Int

rawPageSingleValuePrefix :: RawPage -> SerialisedValue
rawPageSingleValuePrefix page@(RawPage off ba) =
    SerialisedValue $
      RB.fromByteArray
        (mul2 off + fromIntegral start)
        (fromIntegral prefix_end - fromIntegral start)
        ba
  where
    (start, end) = rawPageValueOffsets1 page
    prefix_end   = min 4096 end

rawPageSingleValueSuffix :: RawPage -> Word32
rawPageSingleValueSuffix page
    | end > 4096 = end - 4096
    | otherwise  = 0
  where
    (_, end) = rawPageValueOffsets1 page

-- we could create unboxed array.
{-# INLINE rawPageBlobSpanIndex #-}
rawPageBlobSpanIndex :: RawPage
    -> Int -- ^ blobspan index. Calculate with 'rawPageCalculateBlobIndex'
    -> BlobSpan
rawPageBlobSpanIndex page@(RawPage off ba) i = BlobSpan
    ( indexByteArray ba (off1 + i) )
    ( indexByteArray ba (off2 + i) )
  where
    !dirNumKeys  = rawPageNumKeys page
    !dirNumBlobs = rawPageNumBlobs page

    -- offset to start of blobspan arr
    off1 = div4 off + 1 + ceilDiv64 (fromIntegral dirNumKeys) + ceilDiv64 (fromIntegral (mul2 dirNumKeys))
    off2 = mul2 (off1 + fromIntegral dirNumBlobs)

rawPageCalculateBlobIndex
    :: RawPage
    -> Int  -- ^ key index
    -> Int  -- ^ blobspan index
rawPageCalculateBlobIndex (RawPage off ba) i = do
    let j = unsafeShiftR i 6 -- `div` 64
    let k = i .&. 63         -- `mod` 64
    -- generic sum isn't too great
    let s = foldl' (+) 0 [ popCount (indexByteArray ba (div4 off + 1 + jj) :: Word64) | jj <- [0 .. j-1 ] ]
    let word = indexByteArray ba (div4 off + 1 + j) :: Word64
    s + popCount (word .&. complement (unsafeShiftL 0xffffffffffffffff k))
