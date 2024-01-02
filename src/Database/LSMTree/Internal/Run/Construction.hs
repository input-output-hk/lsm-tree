{- HLINT ignore "Use camelCase" -}

-- TODO: remove once the implementation of incremental run construction is in
-- place
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Incremental, in-memory run consruction
--
module Database.LSMTree.Internal.Run.Construction (
    SerialisedValue
  , SerialisedBlob
  , SerialisedPage
    -- * Incremental, in-memory run construction
    -- $incremental-run-construction
  , MRun
  , new
  , end
    -- ** Add k\/op pairs with smaller-than-page values
  , AddOne
  , add_insert
  , add_insertWithBlob
  , add_mupsert
  , add_delete
    -- ** Add k\/op pairs with larger-than-page values
    -- $larger-than-page
  , AddMany
  , add_insert_largerThanPage
  , add_insertWithBlob_largerThanPage
  , add_mupsert_largerThanPage
    -- *** Optimise for merging runs #optimise-for-merge#
    -- $optimise-for-merge
  , add_insert_largerThanPage'
  , add_insertWithBlob_largerThanPage'
  ) where

import           Control.Monad.ST.Strict
import           Data.List.NonEmpty
import           Data.Primitive.ByteArray (ByteArray)
import           Data.Word
import           Database.LSMTree.Internal.BlobRef (BlobRef)
import           Database.LSMTree.Internal.Run.BloomFilter (Bloom, MBloom)
import qualified Database.LSMTree.Internal.Run.BloomFilter as Bloom
import           Database.LSMTree.Internal.Run.Index.Compact (CompactIndex,
                     MCompactIndex)
import qualified Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise (SerialisedKey)

{-------------------------------------------------------------------------------
  Placeholder types

  TODO: replace by non-placeholder types
-------------------------------------------------------------------------------}

-- | A string of bytes representing a value.
--
-- This representation can also double as a /chunk/ of a complete serialised
-- value, in case the complete serialised value does not fit into a single page.
data SerialisedValue

-- | A string of bytes representing a blob, of arbitrary size
data SerialisedBlob

-- | A string of bytes representing a page, typically 4K in size
data SerialisedPage

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
      bloom :: MBloom s SerialisedKey
    , index :: MCompactIndex s
      -- TODO: keep track of the current, unfinished page. I'm not sure yet in
      -- what form that should be.
    }

-- | @'new' npages@ starts an incremental run construction.
--
-- @npages@ should be an upper bound on the number of pages that will be yielded
-- by incremental run construction.
new :: Int -> ST s (MRun s)
new = undefined

-- | Finalise an incremental run construction.
--
-- The frozen bloom filter and compact index will be returned, along with the
-- final page of the run (if necessary), and the remaining chunks of the
-- incrementally constructed compact index.
end ::
     MRun s
  -> ST s ( Maybe SerialisedPage
          , Index.Chunk
          , Index.FinalChunk
          , Bloom SerialisedKey
          , CompactIndex
          )
end = undefined

{-------------------------------------------------------------------------------
  Smaller-than-page values
-------------------------------------------------------------------------------}

-- | A function that can yield /at most one/ serialised page.
type AddOne s = MRun s -> ST s (Maybe SerialisedPage, Maybe Index.Chunk)

-- Add a serialised Insert operation, which must fit on a single page (including
-- per-entry and per-page overhead).
add_insert :: SerialisedKey -> SerialisedValue -> AddOne s
add_insert = undefined

-- Add a serialised Insert operation with a blob reference, which must fit on a
-- single page (including per-entry and per-page overhead).
add_insertWithBlob ::
     SerialisedKey
  -> SerialisedValue
  -> BlobRef SerialisedBlob
  -> AddOne s
add_insertWithBlob = undefined

-- Add a serialised Mupsert operation, which must fit on a single page
-- (including per-entry and per-page overhead)
add_mupsert :: SerialisedKey -> SerialisedValue -> AddOne s
add_mupsert = undefined

-- Add a serialised Delete operation, which must fit on a single page (including
-- per-entry and per-page overhead).
add_delete :: SerialisedKey  -> AddOne s
add_delete = undefined

{-------------------------------------------------------------------------------
  Larger-than-page values
-------------------------------------------------------------------------------}

{- $larger-than-page

  Larger-than-page values can only be associated with Insert or Mupsert
  operations, and they require special handling.

  There are two use cases for incremental run construction: flushing the write
  buffer, and merging runs. In the latter case, we can take a shortcut to
  optimise run construction because the larger-than-page value is already
  formatted as a set of contiguous pages. See See [Optimise for merging
  runs](#optimise-for-merge#).
-}

-- | A function that can yield /at least one/ serialised page.
type AddMany s = MRun s -> ST s (NonEmpty SerialisedPage, [Index.Chunk])

-- | Like 'add_insert', but for a value that does not fit into a single page.
--
-- To be used /only/ when flushing and writing out the write buffer. When
-- merging runs, use 'add_insert_largerThanPage'' instead.
add_insert_largerThanPage ::
     SerialisedKey
  -> SerialisedValue
  -> AddMany s
add_insert_largerThanPage = undefined

-- | Like 'add_insertWithBlob', but for a value that does not fit into a single
-- page.
--
-- To be used /only/ when flushing and writing out the write buffer. When
-- merging runs, use 'add_insertWithBlob_largerThanPage'' instead.
add_insertWithBlob_largerThanPage ::
     SerialisedKey
  -> SerialisedValue
  -> BlobRef SerialisedBlob
  -> AddMany s
add_insertWithBlob_largerThanPage = undefined

-- | Like 'add_mupsert', but for a value that does not fit into a single page.
--
-- To be used when flushing and writing out the write buffer, or when merging
-- runs.
add_mupsert_largerThanPage ::
     SerialisedKey
  -> SerialisedValue
  -> AddMany s
add_mupsert_largerThanPage = undefined

{- $optimise-for-merge

  We can optimise for the scenario where we are merging runs and we encounter a
  larger-than-page value that we want to include in the output run. In this
  case, we can almost directly copy the pages with miminal changes to the raw
  bytes. Note that this is not true for Mupsert operations, where the output
  value is a combination of multiple serialised values.

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
-- To be used /only/ when merging runs. See [Optimise for merging
-- runs](#optimise-for-merge#).
add_insert_largerThanPage' ::
     [SerialisedPage]
  -> AddMany s
add_insert_largerThanPage' = undefined

-- | An optimised version of adding a larger-than-page value with a blob
-- reference, if it is already formatted as serialised pages.
--
-- To be used /only/ when merging runs. See [Optimise for merging
-- runs](#optimise-for-merge#).
--
-- Note: the blob reference inside the first serialised page will be overwritten
-- by the blob reference argument to this function.
add_insertWithBlob_largerThanPage' ::
     [SerialisedPage]
  -> BlobRef SerialisedBlob
  -> AddMany s
add_insertWithBlob_largerThanPage' = undefined
