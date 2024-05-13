{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Redundant lambda" -}
{- HLINT ignore "Use camelCase" -}

-- | Incremental (in-memory portion of) run consruction
--
module Database.LSMTree.Internal.RunAcc (
    RunAcc
  , new
  , unsafeFinalise
    -- * Adding key\/op pairs
    -- | There are a few variants of actions to add key\/op pairs to the run
    -- accumulator. Which one to use depends on a couple questions:
    --
    -- * Is it fully in memory or is it pre-serialised and only partly in
    --   memory?
    -- * Is the key\/op pair known to be \"small\" or \"large\"?
    --
    -- If it's in memory but it's not known whether it's small or large then
    -- use 'addKeyOp'. One can use 'entryWouldFitInPage' to find out if it's
    -- small or large. If it's in memory and known to be small or large then
    -- use 'addSmallKeyOp' or 'addLargeKeyOp' as appropriate. If it's large
    -- and pre-serialised, use 'addLargeSerialisedKeyOp' but note its
    -- constraints carefully.
    --
  , addKeyOp
  , addSmallKeyOp
  , addLargeKeyOp
  , addLargeSerialisedKeyOp
  , PageAcc.entryWouldFitInPage
  ) where

import           Control.Exception (assert)
import           Control.Monad.ST.Strict
import           Data.BloomFilter (Bloom, MBloom)
import qualified Data.BloomFilter as Bloom
import qualified Data.BloomFilter.Easy as Bloom.Easy
import qualified Data.BloomFilter.Mutable as MBloom
import           Data.Maybe (fromMaybe)
import           Data.Primitive.PrimVar (PrimVar, modifyPrimVar, newPrimVar,
                     readPrimVar, writePrimVar)
import           Data.Word (Word16)
import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import           Database.LSMTree.Internal.Entry (Entry (..), NumEntries (..))
import           Database.LSMTree.Internal.IndexCompact (IndexCompact, NumPages)
import qualified Database.LSMTree.Internal.IndexCompact as Index
import           Database.LSMTree.Internal.IndexCompactAcc (IndexCompactAcc)
import qualified Database.LSMTree.Internal.IndexCompactAcc as Index
import           Database.LSMTree.Internal.PageAcc (PageAcc)
import qualified Database.LSMTree.Internal.PageAcc as PageAcc
import qualified Database.LSMTree.Internal.PageAcc1 as PageAcc
import           Database.LSMTree.Internal.RawOverflowPage
import           Database.LSMTree.Internal.RawPage (RawPage)
import qualified Database.LSMTree.Internal.RawPage as RawPage
import           Database.LSMTree.Internal.Serialise (SerialisedKey,
                     SerialisedValue, keyTopBits16)

{-------------------------------------------------------------------------------
  Incremental, in-memory run construction
-------------------------------------------------------------------------------}

-- | The run accumulator is a mutable structure that accumulates key\/op pairs.
-- It yields pages and chunks of the index incrementally, and returns the
-- Bloom filter and complete index at the end.
--
-- Use 'new' to start run construction, add new key\/operation pairs to the run
-- by using 'addKeyOp' and co, and complete run construction using
-- 'unsafeFinalise'.
data RunAcc s = RunAcc {
      mbloom               :: !(MBloom s SerialisedKey)
    , mindex               :: !(IndexCompactAcc s)
    , mpageacc             :: !(PageAcc s)
    , entryCount           :: !(PrimVar s Int)
    , rangeFinderCurVal    :: !(PrimVar s Word16)
    , rangeFinderPrecision :: !Index.RFP
    }

-- | @'new' npages@ starts an incremental run construction.
--
-- @nentries@ and @npages@ should be an upper bound on the expected number of
-- entries and pages in the output run.
new :: NumEntries
    -> NumPages
    -> Maybe Index.RFP -- ^ For testing: override the default RFP
                       -- which is based on the 'NumPages'.
    -> ST s (RunAcc s)
new (NumEntries nentries) npages rangeFinderPrecisionOverride = do
    mbloom <- Bloom.Easy.easyNew 0.1 nentries -- TODO(optimise): tune bloom filter
    let rangeFinderPrecisionDefault = Index.suggestRangeFinderPrecision npages
        rangeFinderPrecision        = fromMaybe rangeFinderPrecisionDefault
                                                rangeFinderPrecisionOverride
    mindex <- Index.new rangeFinderPrecision (Index.unsafeMkChunkSize 100) -- TODO(optimise): tune chunk size
    mpageacc <- PageAcc.newPageAcc
    entryCount <- newPrimVar 0
    rangeFinderCurVal <- newPrimVar 0
    pure RunAcc{..}

-- | Finalise an incremental run construction. Do /not/ use a 'RunAcc' after
-- finalising it.
--
-- The frozen bloom filter and compact index will be returned, along with the
-- final page of the run (if necessary), and the remaining chunks of the
-- incrementally constructed compact index.
unsafeFinalise ::
     RunAcc s
  -> ST s ( Maybe RawPage
          , Maybe Index.Chunk
          , Bloom SerialisedKey
          , IndexCompact
          , NumEntries
          )
unsafeFinalise racc@RunAcc {..} = do
    mpagemchunk <- flushPageIfNonEmpty racc
    (mchunk', index) <- Index.unsafeEnd mindex
    bloom <- Bloom.unsafeFreeze mbloom
    numEntries <- readPrimVar entryCount
    let !mpage  = fst <$> mpagemchunk
        !mchunk = selectChunk mpagemchunk mchunk'
    pure (mpage, mchunk, bloom, index, NumEntries numEntries)
  where
    selectChunk :: Maybe (RawPage, Maybe Index.Chunk)
                -> Maybe Index.Chunk
                -> Maybe Index.Chunk
    selectChunk (Just (_page, Just _chunk)) (Just _chunk') =
        -- If flushing the page accumulator gives us an index chunk then
        -- the index can't have any more chunks when we finalise the index.
        error "unsafeFinalise: impossible double final chunk"
    selectChunk (Just (_page, Just chunk)) _ = Just chunk
    selectChunk _ (Just chunk)               = Just chunk
    selectChunk _ _                          = Nothing

-- | Add a key\/op pair with an optional blob span to the run accumulator.
--
-- Note that this version expects the full value to be in the given'Entry', not
-- just a prefix of the value that fits into a single page.
--
-- If the key\/op pair is known to be \"small\" or \"large\" then you can use
-- the special versions 'addSmallKeyOp' or 'addLargeKeyOp'. If it is
-- pre-serialised, use 'addLargeSerialisedKeyOp'.
--
addKeyOp
  :: RunAcc s
  -> SerialisedKey
  -> Entry SerialisedValue BlobSpan -- ^ the full value, not just a prefix
  -> ST s ([RawPage], [RawOverflowPage], [Index.Chunk])
addKeyOp racc k e
  | PageAcc.entryWouldFitInPage k e = smallToLarge <$> addSmallKeyOp racc k e
  | otherwise                       =                  addLargeKeyOp racc k e
  where
    smallToLarge :: Maybe (RawPage, Maybe Index.Chunk)
                 -> ([RawPage], [RawOverflowPage], [Index.Chunk])
    smallToLarge Nothing                   = ([],     [], [])
    smallToLarge (Just (page, Nothing))    = ([page], [], [])
    smallToLarge (Just (page, Just chunk)) = ([page], [], [chunk])

-- | Add a \"small\" key\/op pair with an optional blob span to the run
-- accumulator.
--
-- This version is /only/ for small entries that can fit within a single page.
-- Use 'addLargeKeyOp' if the entry is bigger than a page. If this distinction
-- is not known at the use site, use 'PageAcc.entryWouldFitInPage' to determine
-- which case applies, or use 'addKeyOp'.
--
-- This is guaranteed to add the key\/op, and it may yield (at most one) page.
--
addSmallKeyOp
  :: RunAcc s
  -> SerialisedKey
  -> Entry SerialisedValue BlobSpan
  -> ST s (Maybe (RawPage, Maybe Index.Chunk))
addSmallKeyOp racc@RunAcc{..} k e =
  assert (PageAcc.entryWouldFitInPage k e) $ do
    modifyPrimVar entryCount (+1)
    MBloom.insert mbloom k

    -- We have to force a page boundary when the range finder bits change.
    -- This is a constraint from the compact index. To do this we remember the
    -- range finder bits of the previously added key and compare them to the
    -- range finder bits of the current key.
    rfbits <- readPrimVar rangeFinderCurVal -- previous
    let !rfbits' = keyTopBits16 (fromIntegral $ Index.unRFP rangeFinderPrecision) k  -- current

    pageBoundaryNeeded <-
      if rfbits' == rfbits
        -- If the range finder bits didn't change, try adding the key/op to
        -- the page accumulator to see if it fits. If it does not fit, a page
        -- boundary is needed.
        then not <$> PageAcc.pageAccAddElem mpageacc k e

        -- If the rfbits did change we do need a page boundary.
        else writePrimVar rangeFinderCurVal rfbits'
          >> return True

    if pageBoundaryNeeded
      then do
        -- We need a page boundary. If the current page is empty then we have
        -- a boundary already, otherwise we need to flush the current page.
        mpagemchunk <- flushPageIfNonEmpty racc
        -- The current page is now empty, either because it was already empty
        -- or because we just flushed it. Adding the new key/op to an empty
        -- page must now succeed, because we know it fits in a page.
        added <- PageAcc.pageAccAddElem mpageacc k e
        assert added $ return mpagemchunk

      else return Nothing

-- | Add a \"large\" key\/op pair with an optional blob span to the run
-- accumulator.
--
-- This version is /only/ for large entries that span multiple pages. Use
-- 'addSmallKeyOp' if the entry is smaller than a page. If this distinction
-- is not known at the use site, use 'PageAcc.entryWouldFitInPage' to determine
-- which case applies.
--
-- Note that this version expects the full large value to be in the given
-- 'Entry', not just the prefix of the value that fits into a single page.
-- For large multi-page values that are represented by a pre-serialised
-- 'RawPage' (as occurs when merging runs), use 'addLargeSerialisedKeyOp'.
--
-- This is guaranteed to add the key\/op. It will yield one or two 'RawPage's,
-- and one or more 'RawOverflowPage's. These pages should be written out to
-- the run's page file in that order, the 'RawPage's followed by the
-- 'RawOverflowPage's.
--
addLargeKeyOp
  :: RunAcc s
  -> SerialisedKey
  -> Entry SerialisedValue BlobSpan -- ^ the full value, not just a prefix
  -> ST s ([RawPage], [RawOverflowPage], [Index.Chunk])
addLargeKeyOp racc@RunAcc{..} k e =
  assert (not (PageAcc.entryWouldFitInPage k e)) $ do
    modifyPrimVar entryCount (+1)
    MBloom.insert mbloom k

    -- If the existing page accumulator is non-empty, we flush it, since the
    -- new large key/op will need more than one page to itself.
    mpagemchunkPre <- flushPageIfNonEmpty racc

    -- Make the new page and overflow pages. Add the span of pages to the index.
    let (page, overflowPages) = PageAcc.singletonPage k e
    chunks <- Index.appendMulti (k, fromIntegral (length overflowPages)) mindex

    -- Combine the results with anything we flushed before
    let (!pages, !chunks') = selectPagesAndChunks mpagemchunkPre page chunks
    return (pages, overflowPages, chunks')

-- | Add a \"large\" pre-serialised key\/op entry to the run accumulator.
--
-- This version is for large entries that span multiple pages and are
-- represented by already serialised 'RawPage' and one or more
-- 'RawOverflowPage's.
--
-- For this case, the caller provides the key, the raw page it is from and the
-- overflow pages. The raw page and overflow pages are returned along with any
-- other pages that need to be yielded (in order). The caller should write out
-- the pages to the run's page file in order: the returned 'RawPage's followed
-- by the 'RawOverflowPage's (the same as for 'addLargeKeyOp').
--
-- Note that this action is not appropriate for key\/op entries that would fit
-- within a page ('PageAcc.entryWouldFitInPage') but just /happen/ to have
-- ended up in a page on their own in an input to a merge. A page can end up
-- with a single entry because a page boundary was needed rather than because
-- the entry itself was too big. Furthermore, pre-serialised pages can only be
-- used unaltered if the entry does /not/ use a 'BlobSpan', since the 'BlobSpan'
-- typically needs to be modified. Thus the caller should use the following
-- tests to decide if 'addLargeSerialisedKeyOp' should be used:
--
-- 1. The entry does not use a 'BlobSpan'.
-- 2. The entry definitely overflows onto one or more overflow pages.
--
-- Otherwise, use 'addLargeKeyOp' or 'addSmallKeyOp' as appropriate.
--
addLargeSerialisedKeyOp
  :: RunAcc s
  -> SerialisedKey     -- ^ The key
  -> RawPage           -- ^ The page that this key\/op is in, which must be the
                       -- first page of a multi-page representation of a single
                       -- key\/op /without/ a 'BlobSpan'.
  -> [RawOverflowPage] -- ^ The overflow pages for this key\/op
  -> ST s ([RawPage], [RawOverflowPage], [Index.Chunk])
addLargeSerialisedKeyOp racc@RunAcc{..} k page overflowPages =
  assert (RawPage.rawPageNumKeys page == 1) $
  assert (RawPage.rawPageHasBlobSpanAt page 0 == 0) $
  assert (RawPage.rawPageOverflowPages page > 0) $
  assert (RawPage.rawPageOverflowPages page == length overflowPages) $ do
    modifyPrimVar entryCount (+1)
    MBloom.insert mbloom k

    -- If the existing page accumulator is non-empty, we flush it, since the
    -- new large key/op will need more than one page to itself.
    mpagemchunkPre <- flushPageIfNonEmpty racc
    let nOverflowPages = length overflowPages --TODO: consider using vector
    chunks <- Index.appendMulti (k, fromIntegral nOverflowPages) mindex
    let (!pages, !chunks') = selectPagesAndChunks mpagemchunkPre page chunks
    return (pages, overflowPages, chunks')

-- | Internal helper: finalise the current page, add the page to the index,
-- reset the page accumulator and return the serialised 'RawPage' along with
-- any index chunk.
--
-- Returns @Nothing@ if the page accumulator was empty.
--
flushPageIfNonEmpty :: RunAcc s -> ST s (Maybe (RawPage, Maybe Index.Chunk))
flushPageIfNonEmpty RunAcc{mpageacc, mindex} = do
    nkeys <- PageAcc.keysCountPageAcc mpageacc
    if nkeys > 0
      then do
        -- Grab the min and max keys, and add the page to the index.
        minKey <- PageAcc.indexKeyPageAcc mpageacc 0
        maxKey <- PageAcc.indexKeyPageAcc mpageacc (nkeys-1)
        mchunk <- Index.appendSingle (minKey, maxKey) mindex

        -- Now serialise the page and reset the accumulator
        page <- PageAcc.serialisePageAcc mpageacc
        PageAcc.resetPageAcc mpageacc
        return (Just (page, mchunk))

      else pure Nothing

-- | Internal helper for 'addLargeKeyOp' and 'addLargeSerialisedKeyOp'.
-- Combine the result of 'flushPageIfNonEmpty' with extra pages and index
-- chunks.
--
selectPagesAndChunks :: Maybe (RawPage, Maybe Index.Chunk)
                     -> RawPage
                     -> [Index.Chunk]
                     -> ([RawPage], [Index.Chunk])
selectPagesAndChunks mpagemchunkPre page chunks =
  case mpagemchunkPre of
    Nothing                       -> (         [page],          chunks)
    Just (pagePre, Nothing)       -> ([pagePre, page],          chunks)
    Just (pagePre, Just chunkPre) -> ([pagePre, page], chunkPre:chunks)

