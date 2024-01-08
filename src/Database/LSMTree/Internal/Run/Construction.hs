{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
{- HLINT ignore "Use camelCase" -}

-- | Incremental, in-memory run consruction
--
module Database.LSMTree.Internal.Run.Construction (
    -- * Placeholder types
    SerialisedValue
  , SerialisedPages
    -- * Incremental, in-memory run construction
    -- $incremental-run-construction
  , MRun
  , new
  , end
    -- ** Adding k\/op pairs
  , Add
  , KOp
  , add_insert
  , add_insertWithBlob
  , add_mupsert
  , add_delete
    -- ** Add k\/op pairs with larger-than-page values
    -- $larger-than-page
  , add_insert_largerThanPage
  , add_insertWithBlob_largerThanPage
  ) where

import           Control.Monad.ST.Strict
import           Data.Bits
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Short.Internal (ShortByteString (SBS))
import           Data.Foldable
import qualified Data.List as List
import           Data.Maybe (fromJust, isJust)
import           Data.Primitive.ByteArray
import           Data.STRef
import           Data.Word
import           Database.LSMTree.Internal.Run.BloomFilter (Bloom, MBloom)
import qualified Database.LSMTree.Internal.Run.BloomFilter as Bloom
import           Database.LSMTree.Internal.Run.Index.Compact (Chunk,
                     CompactIndex, MCompactIndex)
import qualified Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise (SerialisedKey (..))

{-------------------------------------------------------------------------------
  Placeholder types

  TODO: replace by non-placeholder types
-------------------------------------------------------------------------------}

-- | A string of bytes representing one or more disk pages.
--
-- Typically, this type only represents a single page, unless it is the result
-- of adding a larger-than-page value to a run.
--
-- Note: the string of bytes is not padded to the target disk-page size. The
-- consumer of this type should ensure correct padding when writing out the
-- byte string.
newtype SerialisedPages = SerialisedPages LBS.ByteString

-- | A string of bytes representing a value.
newtype SerialisedValue = SerialisedValue ByteArray

-- | Size of key in number of bytes.
sizeofKey :: SerialisedKey -> Int
sizeofKey (SerialisedKey ba) = sizeofByteArray ba

-- | Size of key in number of bytes.
sizeofKey16 :: SerialisedKey -> Word16
sizeofKey16 = fromIntegral . sizeofKey

-- | Size of key in number of bytes.
sizeofKey64 :: SerialisedKey -> Word64
sizeofKey64 = fromIntegral . sizeofKey

-- | Size of value in number of bytes.
sizeofValue :: SerialisedValue -> Int
sizeofValue (SerialisedValue ba) = sizeofByteArray ba

-- | Size of value in number of bytes.
sizeofValue16 :: SerialisedValue -> Word16
sizeofValue16 = fromIntegral . sizeofValue

-- | Size of value in number of bytes.
sizeofValue32 :: SerialisedValue -> Word32
sizeofValue32 = fromIntegral . sizeofValue

-- | Size of value in number of bytes.
sizeofValue64 :: SerialisedValue -> Word64
sizeofValue64 = fromIntegral . sizeofValue

emptyValue :: SerialisedValue
emptyValue = SerialisedValue emptyByteArray

{-------------------------------------------------------------------------------
  TODO: placeholders until #50 is merged
-------------------------------------------------------------------------------}

data BlobRef blob = BlobRef {
    blobRefOffset :: !Word64
  , blobRefSize   :: !Word32
  }

{-------------------------------------------------------------------------------
  TODO: placeholders until #55 is merged
-------------------------------------------------------------------------------}

-- | Min\/max key-info for pages
data Append =
    -- | One or more keys are in this page, and their values fit within a single
    -- page.
    AppendSinglePage SerialisedKey SerialisedKey
    -- | There is only one key in this page, and it's value does not fit within
    -- a single page.
  | AppendMultiPage SerialisedKey Word32 -- ^ Number of overflow pages

-- | Append a new page entry to a mutable compact index.
append :: Append -> MCompactIndex s -> ST s [Chunk]
append = error "append: replace by Database.LSMTree.Internal.Run.Index.Compact.append"

{-------------------------------------------------------------------------------
  Incremental, in-memory run construction
-------------------------------------------------------------------------------}

{- $incremental-run-construction

  When smaller-than-page values are added and the current page becomes full,
  then a serialised page will be yielded. When a chunk of the incrementally
  constructed compact index becomes full, then that chunk will also be yielded.

  For larger-than-page values, multiple (at least one) serialised pages will be
  yielded. Any number of compact index chunks could be yielded too.
-}

-- | A mutable structure that keeps track of incremental, in-memory run
-- construction.
data MRun s = MRun {
      mbloom         :: !(MBloom s SerialisedKey)
    , mindex         :: !(MCompactIndex s)
    , indexChunksRef :: !(STRef s [[Index.Chunk]])
    , currentPageRef :: !(STRef s PageIntermediate)
    }

-- | @'new' npages@ starts an incremental run construction.
--
-- @npages@ should be an upper bound on the number of pages that will be yielded
-- by incremental run construction.
new :: Int -> ST s (MRun s)
new npages = do
    mbloom <- Bloom.newEasy 0.1 npages -- TODO: tune bloom filter
    let rfprec = Index.suggestRangeFinderPrecision npages
    mindex <- Index.new rfprec 100 -- TODO(optimise): tune chunk size
    indexChunksRef <- newSTRef []
    currentPageRef <- newSTRef pageIntermediateEmpty
    pure MRun{..}

-- | Finalise an incremental run construction. Do /not/ use the 'MRun' after
-- finalising it.
--
-- The frozen bloom filter and compact index will be returned, along with the
-- final page of the run (if necessary), and the remaining chunks of the
-- incrementally constructed compact index.
end ::
     MRun s
  -> ST s ( Maybe SerialisedPages
          , Index.Chunk
          , Index.FinalChunk
          , Bloom SerialisedKey
          , CompactIndex
          )
end MRun {..} = do
    p <- readSTRef currentPageRef
    mlastPage <-
      if pageIntermediateIsEmpty p then
        pure Nothing
      else do
        newChunks <- append (mkAppend p) mindex
        modifySTRef' indexChunksRef $ \chunkss -> newChunks : chunkss
        pure $ Just $ serialisePage p
    (chunk, fchunk) <- Index.unsafeEnd mindex
    bloom <- Bloom.freeze mbloom
    chunkss <- readSTRef indexChunksRef
    let chunks = concat (reverse ([chunk] : chunkss))
        index = Index.fromChunks chunks fchunk
    pure (mlastPage, chunk, fchunk, bloom, index)

{-------------------------------------------------------------------------------
  Adding k\/op pairs
-------------------------------------------------------------------------------}

-- | A function that can yield /at most one/ serialised page.
type Add s = MRun s -> ST s (Maybe SerialisedPages, [Index.Chunk])
-- | A k\/op pair with an optional blob reference.
type KOp blob = (SerialisedKey, Operation, Maybe (BlobRef blob))

-- Add a serialised k\/op pair with an optional blob reference.
add :: KOp blob -> Add s
add kop@(k0, _, _) MRun{..} = do
    Bloom.insert mbloom k0
    p <- readSTRef currentPageRef
    case pageIntermediateAddElem kop p of
      Nothing -> do
        newChunks <- append (mkAppend p) mindex
        writeSTRef currentPageRef $! pageIntermediateSingleton kop
        modifySTRef' indexChunksRef $ \chunkss -> newChunks : chunkss
        pure (Just $ serialisePage p, newChunks)
      Just p' -> do
        writeSTRef currentPageRef $! p'
        pure (Nothing, [])

-- Add a serialised Insert operation.
add_insert :: SerialisedKey -> SerialisedValue -> Add s
add_insert k v = add (k, Insert v, Nothing)

-- Add a serialised Insert operation with a blob reference.
add_insertWithBlob ::
     SerialisedKey
  -> SerialisedValue
  -> BlobRef blob
  -> Add s
add_insertWithBlob k v b = add (k, Insert v, Just b)

-- Add a serialised Mupsert operation.
add_mupsert :: SerialisedKey -> SerialisedValue -> Add s
add_mupsert k v = add (k, Mupsert v, Nothing)

-- Add a serialised Delete operation.
add_delete :: SerialisedKey  -> Add s
add_delete k = add (k, Delete, Nothing)

{-------------------------------------------------------------------------------
  Larger-than-page values
-------------------------------------------------------------------------------}

{- $larger-than-page

  There are two use cases for incremental run construction: flushing the write
  buffer, and merging runs. In the latter case, we can take a shortcut to
  optimise run construction because the larger-than-page value is already
  formatted as a set of contiguous pages. In this case, we can almost directly
  copy the pages with miminal changes to the raw bytes. Note that this is not
  true for Mupsert operations, where the output value is a combination of
  multiple serialised values.

  If a serialised value does not fit into a page, than multiple pages are read.
  The complete serialised value is compactly divided over these pages.

  * The first page will list the start and end offset of the large value. These
    offsets are relative to the first page. It also contains the serialised key
    and the first chunk of the complete serialised value. It might also contain
    offsets for a blob reference.

  * Overflow pages after the first page contain only the raw bytes that make up
    the other chunks of the complete serialised value.

  * The last page has padding after the offset where the large value ends.

  The only part of the serialised page that has to change is the offsets for the
  blob reference.
-}

-- | An optimised version of adding a larger-than-page value, if it is already
-- formatted as serialised pages.
--
-- TODO: implement
add_insert_largerThanPage ::
     SerialisedPages
  -> Add s
add_insert_largerThanPage = undefined

-- | An optimised version of adding a larger-than-page value with a blob
-- reference, if it is already formatted as serialised pages.
--
-- Note: the blob reference inside the first serialised page will be overwritten
-- by the blob reference argument to this function.
--
-- TODO: implement
add_insertWithBlob_largerThanPage ::
     SerialisedPages
  -> BlobRef blob
  -> Add s
add_insertWithBlob_largerThanPage = undefined

{-------------------------------------------------------------------------------
  Intermediate representation for a page
-------------------------------------------------------------------------------}

data OperationEnum = OpInsert | OpMupsert | OpDelete
  deriving (Eq, Show)

data Operation =
    Insert SerialisedValue
  | Mupsert SerialisedValue
  | Delete

toOperationEnum :: Operation -> OperationEnum
toOperationEnum Insert{}  = OpInsert
toOperationEnum Mupsert{} = OpMupsert
toOperationEnum Delete{}  = OpDelete

toValue :: Operation -> SerialisedValue
toValue (Insert v)  = v
toValue (Mupsert v) = v
toValue Delete      = emptyValue

--
-- PageIntermediate
--

data PageIntermediate = PageIntermediate {
    -- | (1) directory of components
    pageSize           :: !PageSize
    -- | (2) an array of 1-bit blob reference indicators
    --
    -- TODO(optimise): change to @['Word64']@
  , pageBlobRefBitmap  :: ![Bool]
    -- | (3) an array of 2-bit operation types
    --
    -- TODO(optimise): change to @['Word64']@
  , pageOperations     :: ![OperationEnum]
    -- | (4) a pair of arrays of blob references
  , pageBlobRefOffsets :: ![Word64]
  , pageBlobRefSizes   :: ![Word32]
    -- | (7) the concatenation of all keys
  , pageKeys           :: ![SerialisedKey]
    -- | (8) the concatenation of all values
  , pageValues         :: ![SerialisedValue]
  }

pageIntermediateIsEmpty :: PageIntermediate -> Bool
pageIntermediateIsEmpty = (==pageSizeEmpty) . pageSize

pageIntermediateEmpty :: PageIntermediate
pageIntermediateEmpty = PageIntermediate {
      pageSize = pageSizeEmpty
    , pageBlobRefBitmap = []
    , pageOperations = []
    , pageBlobRefOffsets = []
    , pageBlobRefSizes = []
    , pageKeys = []
    , pageValues = []
    }

pageIntermediateAddElem ::
     KOp blob
  -> PageIntermediate
  -> Maybe PageIntermediate
pageIntermediateAddElem kop@(k, op, mblobref) PageIntermediate{..}
  | Just pgsz' <- pageSizeAddElem kop pageSize = Just $ PageIntermediate {
        pageSize = pgsz'
      , pageBlobRefBitmap = isJust mblobref : pageBlobRefBitmap
      , pageOperations = toOperationEnum op : pageOperations
      , pageBlobRefOffsets = case mblobref of
            Nothing          -> pageBlobRefOffsets
            Just BlobRef{..} -> blobRefOffset : pageBlobRefOffsets
      , pageBlobRefSizes = case mblobref of
            Nothing          -> pageBlobRefSizes
            Just BlobRef{..} -> blobRefSize : pageBlobRefSizes
      , pageKeys = k : pageKeys
      , pageValues = toValue op : pageValues
      }
  | otherwise = Nothing

pageIntermediateSingleton :: KOp blob -> PageIntermediate
pageIntermediateSingleton kop = fromJust $
    pageIntermediateAddElem kop pageIntermediateEmpty

pageIntermediateReverse :: PageIntermediate -> PageIntermediate
pageIntermediateReverse PageIntermediate{..} = PageIntermediate {
      pageSize
    , pageBlobRefBitmap = reverse pageBlobRefBitmap
    , pageOperations = reverse pageOperations
    , pageBlobRefOffsets = reverse pageBlobRefOffsets
    , pageBlobRefSizes = reverse pageBlobRefSizes
    , pageKeys = reverse pageKeys
    , pageValues = reverse pageValues
    }

serialisePage :: PageIntermediate -> SerialisedPages
serialisePage (pageIntermediateReverse -> PageIntermediate{..}) =
      SerialisedPages
    . BB.toLazyByteString $
        -- (1) directory of components
           BB.word16LE pageSizeNumElems
        <> BB.word16LE pageSizeNumBlobs
        <> BB.word16LE offKeyOffsets
        <> BB.word16LE 0
        -- (2) an array of 1-bit blob reference indicators
        <> mconcat [ BB.word64LE w | w <- toBitmap pageBlobRefBitmap ]
        -- (3) an array of 2-bit operation types
        <> mconcat [ BB.word64LE w | w <- toBitmap . concatMap opEnumToBits
                                                   $ pageOperations ]
        -- (4) a pair of arrays of blob references
        <> mconcat [ BB.word64LE w64 | w64 <- pageBlobRefOffsets ]
        <> mconcat [ BB.word32LE w32 | w32 <- pageBlobRefSizes ]
        -- (5) an array of key offsets
        <> mconcat [ BB.word16LE off | off <- pageKeyOffsets ]
        -- (6) an array of value offsets
        <> case pageValueOffsets of
             Left   offsets -> mconcat [ BB.word16LE off | off <- offsets ]
             Right (offset1, offset2) -> BB.word16LE offset1
                                      <> BB.word32LE offset2
        -- (7) the concatenation of all keys
        <> mconcat [ BB.shortByteString (SBS ba#)
                   | SerialisedKey (ByteArray ba#) <- pageKeys
                   ]
        -- (8) the concatenation of all values
        <> mconcat [ BB.shortByteString (SBS ba#)
                   | SerialisedValue (ByteArray ba#) <- pageValues
                   ]
  where
    PageSize{..} = pageSize

    n = pageSizeNumElems
    b = pageSizeNumBlobs

    -- Offset to component (5)
    offKeyOffsets =
         8                                         -- size of (1)
      + (n + 63) `shiftR` 3 .&. complement 0x7     -- size of (2)
      + (2 * n + 63) `shiftR` 3 .&. complement 0x7 -- size of (3)
      + (4 + 8) * b                                -- size of (4)

    -- Offset to component (7)
    offKeys =
        offKeyOffsets
      + 2 * n                             -- size of (5)
      + (if n == 1 then 6 else 2 * (n+1)) -- size of (6)

    -- Thes values start right after the keys,
    (pageKeyOffsets, offValues) = unsnoc $
        scanl (\o k -> o + sizeofKey16 k) offKeys pageKeys

    pageValueOffsets = case pageValues of
      [v] -> Right (offValues, fromIntegral offValues + sizeofValue32 v)
      _   -> Left (scanl (\o v -> o + sizeofValue16 v) offValues pageValues)

    toBitmap :: [Bool] -> [Word64]
    toBitmap =
        map toWord64 . group64
      where
        toWord64 :: [Bool] -> Word64
        toWord64 = foldl' (\w (n',b') -> if b' then setBit w n' else w) 0
                . zip [0 :: Int ..]
        group64  = List.unfoldr (\xs -> if null xs
                                    then Nothing
                                    else Just (splitAt 64 xs))

    opEnumToBits OpInsert  = [False, False]
    opEnumToBits OpMupsert = [True,  False]
    opEnumToBits OpDelete  = [False, True]

mkAppend :: PageIntermediate -> Append
mkAppend p = case pageKeys p of
    []                     -> error "mkAppend: empty list"
    [k] | numBytes <= 4096 -> AppendSinglePage k k
        | otherwise        -> AppendMultiPage  k (fromIntegral $ numBytes `rem` 4096 - 1)
    (k:ks)                 -> AppendSinglePage k (last ks)
  where
    numBytes = pageSizeNumBytes $ pageSize p

--
-- PageSize
--

data PageSize = PageSize {
    pageSizeNumElems :: !Word16
  , pageSizeNumBlobs :: !Word16
  , pageSizeNumBytes :: !Word64
  }
  deriving (Eq, Show)

pageSizeEmpty :: PageSize
pageSizeEmpty = PageSize 0 0 10

pageSizeAddElem :: KOp blob -> PageSize -> Maybe PageSize
pageSizeAddElem (k, op, mblobref) (PageSize n b sz)
  | sz' <= 4096 || n' == 1 = Just (PageSize n' b' sz')
  | otherwise              = Nothing
  where
    n' = n+1
    b' | isJust mblobref = b+1
       | otherwise       = b
    sz' = sz
        + (if n `mod` 64 == 0 then 8 else 0)    -- (2) blobrefs bitmap
        + (if n `mod` 32 == 0 then 8 else 0)    -- (3) operations bitmap
        + (if isJust mblobref then 12 else 0)   -- (4) blobref entry
        + 2                                     -- (5) key offsets
        + (case n of { 0 -> 4; 1 -> 0; _ -> 2}) -- (6) value offsets
        + sizeofKey64 k                         -- (7) key bytes
        + (case op of                           -- (8) value bytes
             Insert  v -> sizeofValue64 v
             Mupsert v -> sizeofValue64 v
             Delete    -> 0)

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

unsnoc :: [a] -> ([a], a)
unsnoc []     = error "unsnoc: empty list"
unsnoc [x]    = ([], x)
unsnoc (x:xs) = let (xs', y) = unsnoc xs in (x:xs', y)
