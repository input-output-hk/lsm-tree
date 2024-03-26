{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Database.LSMTree.Internal.Lookup (
    Run
  , prepLookups
    -- * Lookups in IO
  , BatchSize (..)
  , Resolve1
  , lookupsInBatches
  ) where

import           Control.Exception (Exception)
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow (MonadThrow (..))
import           Control.Monad.Primitive (PrimMonad (PrimState))
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Foldable (Foldable (..))
import           Data.Functor.Compose
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Primitive.ByteArray
import qualified Data.Vector.Primitive as P
import           Database.LSMTree.Internal.BlobRef (BlobSpan)
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.RawPage
import           Database.LSMTree.Internal.Run.BloomFilter (Bloom)
import qualified Database.LSMTree.Internal.Run.BloomFilter as Bloom
import           Database.LSMTree.Internal.Run.Index.Compact (CompactIndex,
                     PageSpan (..))
import qualified Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise
import           Database.LSMTree.Internal.Serialise.RawBytes
                     (RawBytes (RawBytes))
import           System.FS.API (BufferOffset (BufferOffset, unBufferOffset),
                     Handle)
import           System.FS.BlockIO.API
import           System.FS.IO (HandleIO)

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

{-------------------------------------------------------------------------------
  Lookups in IO
-------------------------------------------------------------------------------}

{-
  Note [Batched lookups, buffer strategy and restrictions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  As a first implementation, we use a simple buffering strategy for batched
  lookups: allocate fresh buffers, and GC them once they are no longer used. In
  a later phase of development, we will look into more elaborate buffer
  strategies, for example using a cache of page buffers, which reduces the
  number of blocks of memory that we allocate/free.

  When we implement a reusable buffer strategy, we will have to take extra care
  to copy bytes from raw pages when necessary. 'rawPageLookup' slices serialised
  values from pages without copying bytes. If the serialised value survives
  after the reusable buffer is returned to the cache, accessing that serialised
  value's bytes has undefined behaviour __unless__ we ensure that we copy from
  the raw page instead of or in addition to slicing.

  There are currently no alignment constraints on the buffers, but there will be
  in the future. The plan is to optimise file access by having key\/operation
  files opened with the @O_DIRECT@ flag, which requires buffers to be aligned to
  the filesystem block size (typically 4096 bytes). We expect to benefit from
  this flag in the UTxO use case, because UTxO keys are uniformly distributed
  hashes, which means that there is little to no spatial locality when
  performing lookups, and so we can skip looking at the page cache.
-}

newtype BatchSize = BatchSize { unBatchSize :: Int }

-- | A batch lookup read fewer bytes than requested.
data NotEnoughBytes = NotEnoughBytes
  deriving (Show, Exception)

{-# SPECIALIZE lookupsInBatches ::
       HasBlockIO IO HandleIO
    -> Resolve1
    -> BatchSize
    -> [Run (Handle HandleIO)]
    -> [SerialisedKey]
    -> IO [(SerialisedKey, Maybe (Entry SerialisedValue BlobSpan))]
  #-}
-- | Batched lookups.
--
-- When the length of the list of lookup keys exceeds the batch size, the list
-- is subdivided into batches of at most the given batch size. Each batch is
-- submitted concurrently using async IO, which could result in a speedup if
-- this code is run with multi-threading enabled.
--
-- See Note [Batched lookups, buffer strategy and restrictions]
--
-- TODO: optimise by reducing allocations, possibly looking at core, or
-- revisiting the batching approach.
lookupsInBatches ::
     forall m h. (MonadAsync m, PrimMonad m, MonadThrow m)
  => HasBlockIO m h
  -> Resolve1
  -> BatchSize
  -> [Run (Handle h)]
  -> [SerialisedKey]
  -> m [(SerialisedKey, Maybe (Entry SerialisedValue BlobSpan))]
lookupsInBatches hbio resolve1 n runs ks = do
    let prepped = prepLookups runs ks
        (fioops, npages) = prepIOOpReads (Compose prepped)
    mbuf <- newPinnedByteArray (npages * 4096)
    let Compose ioops = fioops mbuf
    let batches = groupsOfN (unBatchSize n) ioops
    kess <- forConcurrently batches $ \batch -> do
      ress <- submitIO hbio (fmap snd batch)
      buf <- unsafeFreezeByteArray mbuf
      forM (zip batch ress) $ \((k, ioop), res) -> do
        unless (checkIOResult ioop res) $ throwIO NotEnoughBytes
        let rpl = rawPageLookup (makeRawPage buf (unBufferOffset $ ioopBufferOffset ioop)) k
        pure . (k,) $ case rpl of
          LookupEntryNotPresent   -> Nothing
          LookupEntry e           ->
            -- Laziness ensures that we only compute the forcing of the value in
            -- the entry when the result is needed.
            Just (forceEntry e)
          LookupEntryOverflow e m ->
            let vOverflow = RawBytes (P.Vector
                                        (unBufferOffset (ioopBufferOffset ioop) + 4096)
                                        (fromIntegral m - 4096)
                                        buf)
            in  -- Laziness ensures that we only compute the appending of the
                -- prefix and suffix when the result is needed. We do not use
                -- 'force' here, since appending already creates a new primary
                -- vector. TODO: verify if appending always computes a new
                -- primary vector
                Just $ first (\(SerialisedValue v) -> SerialisedValue (v <> vOverflow)) e
    pure $ resolves resolve1 kess

forceEntry :: Entry SerialisedValue blobref -> Entry SerialisedValue blobref
forceEntry (Insert v)            = Insert (forceSerialisedValue v)
forceEntry (InsertWithBlob v br) = InsertWithBlob (forceSerialisedValue v) br
forceEntry (Mupdate v)           = Mupdate (forceSerialisedValue v)
forceEntry e@Delete              = e

forceSerialisedValue :: SerialisedValue -> SerialisedValue
forceSerialisedValue (SerialisedValue (RawBytes pv)) =
    SerialisedValue (RawBytes (P.force pv))

-- | Prepare 'IOOpRead's up to the argument buffer, and compute the total number
-- of disk pages that we have to allocate a buffer for.
prepIOOpReads ::
     forall m h f. Traversable f
  => f (Handle h, PageSpan)
  -> ( MutableByteArray (PrimState m) -> f (IOOp m h)
     , Index.NumPages
     )
prepIOOpReads pspans = bimap sequence unBufferOffset $ runState (traverse f pspans) 0
  where
    f :: (Handle h, PageSpan)
      -> State BufferOffset (MutableByteArray (PrimState m) -> IOOp m h)
    f (h, pspan) = state $ \off ->
        let size = Index.unPageNo (pageSpanEnd pspan)
                 - Index.unPageNo (pageSpanStart pspan)
        in  ( \buf -> IOOpRead
                h
                (fromIntegral $ Index.unPageNo (pageSpanStart pspan))
                buf
                (off * 4096)
                (fromIntegral $ size * 4096)
            , off + BufferOffset size
            )

groupsOfN :: Int -> [a] -> [[a]]
groupsOfN _ [] = []
groupsOfN n xs = let (ys, zs) = splitAt n xs
                 in  ys : groupsOfN n zs

-- | Check that the IOOp was performed succesfully, and that it wrote/read
-- exactly as many bytes as we expected. If not, then the buffer won't contain
-- the correct number of disk-page bytes.
checkIOResult :: IOOp m h -> IOResult -> Bool
checkIOResult ioop (IOResult m) = ioopByteCount ioop == m

-- | Like @'NonEmpty.groupWith' fst xs@, but distributing out the first part of
-- the pairs.
--
-- PRECONDITION: @sortOn fst xs == xs@
groupWithDistrOut :: Ord a => [(a, b)] -> [(a, NonEmpty b)]
groupWithDistrOut = fmap distrOut . NonEmpty.groupWith fst
  where -- all first parts of the pairs are the same, since we grouped before this
        distrOut ((x, y) :| xys) = (x, y :| fmap snd xys)

type Resolve1 = NonEmpty (Maybe (Entry SerialisedValue BlobSpan))
             -> Maybe (Entry SerialisedValue BlobSpan)

resolves ::
     Resolve1
  -> [[(SerialisedKey, Maybe (Entry SerialisedValue BlobSpan))]]
  -> [(SerialisedKey, Maybe (Entry SerialisedValue BlobSpan))]
resolves f = fmap (second f) . groupWithDistrOut . List.concat

_resolvesNormal ::
     [[(SerialisedKey, Maybe (Entry SerialisedValue BlobSpan))]]
  -> [(SerialisedKey, Maybe (Entry SerialisedValue BlobSpan))]
_resolvesNormal = resolves combinesNormal'

_resolvesMonoidal ::
     (SerialisedValue -> SerialisedValue -> SerialisedValue)
  -> [[(SerialisedKey, Maybe (Entry SerialisedValue BlobSpan))]]
  -> [(SerialisedKey, Maybe (Entry SerialisedValue BlobSpan))]
_resolvesMonoidal f = resolves (combinesMonoidal' f)
