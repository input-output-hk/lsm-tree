{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Database.LSMTree.Internal.Lookup (
    -- * Lookup preparation
    RunIx
  , KeyIx
  , prepLookups
  , bloomQueries
  , bloomQueriesDefault
  , indexSearches
    -- * Lookups in IO
  , ResolveSerialisedValue
  , lookupsIO
  , intraPageLookups
  ) where

import           Control.Exception (Exception, assert)
import           Control.Monad
import           Control.Monad.Class.MonadST as Class
import           Control.Monad.Class.MonadThrow (MonadThrow (..))
import           Control.Monad.Primitive
import           Control.Monad.ST.Strict
import           Data.Bifunctor
import           Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom
import qualified Data.BloomFilter.Hash as Bloom
import           Data.Primitive.ByteArray
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word (Word32)
import           Database.LSMTree.Internal.BlobRef (BlobRef (..))
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.IndexCompact (IndexCompact,
                     PageSpan (..))
import qualified Database.LSMTree.Internal.IndexCompact as Index
import           Database.LSMTree.Internal.RawBytes (RawBytes (..))
import qualified Database.LSMTree.Internal.RawBytes as RB
import           Database.LSMTree.Internal.RawPage
import           Database.LSMTree.Internal.Run (Run)
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.Serialise
import           Database.LSMTree.Internal.Vector (mkPrimVector)
import           System.FS.API (BufferOffset (..), Handle)
import           System.FS.BlockIO.API

-- | Prepare disk lookups by doing bloom filter queries, index searches and
-- creating 'IOOp's. The result is a vector of 'IOOp's and a vector of indexes,
-- both of which are the same length. The indexes record the run and key
-- associated with each 'IOOp'.
prepLookups ::
     V.Vector (Bloom SerialisedKey)
  -> V.Vector IndexCompact
  -> V.Vector (Handle h)
  -> V.Vector SerialisedKey
  -> ST s (VU.Vector (RunIx, KeyIx), V.Vector (IOOp s h))
prepLookups blooms indexes kopsFiles ks = do
  let !rkixs = bloomQueriesDefault blooms ks
  !ioops <- indexSearches indexes kopsFiles ks rkixs
  pure (rkixs, ioops)

type KeyIx = Int
type RunIx = Int
type ResIx = Int -- Result index

-- | 'bloomQueries' with a default result vector size of @V.length ks * 2@.
--
-- The result vector can be of variable length, so we use a generous
-- estimate here, and we grow the vector if needed. TODO: tune the
-- starting estimate based on the expected true- and false-positives.
bloomQueriesDefault ::
     V.Vector (Bloom SerialisedKey)
  -> V.Vector SerialisedKey
  -> VU.Vector (RunIx, KeyIx)
bloomQueriesDefault blooms ks = bloomQueries blooms ks (fromIntegral $ V.length ks * 2)

-- | Perform a batch of bloom queries. The result is a tuple of indexes into the
-- vector of runs and vector of keys respectively.
--
-- Note: the result vector should be ephemeral, so do not retain it.
--
-- TODO: we consider it likely that we could implement a optimised, batched
-- version of bloom filter queries, which would largely replace this function.
bloomQueries ::
     V.Vector (Bloom SerialisedKey)
  -> V.Vector SerialisedKey
  -> Word32
  -> VU.Vector (RunIx, KeyIx)
bloomQueries !blooms !ks !resN
  | rsN == 0 || ksN == 0 = VU.empty
  | otherwise            = VU.create $ do
      res <- VUM.unsafeNew (fromIntegral resN)
      loop1 res 0 0
  where
    !rsN = V.length blooms
    !ksN = V.length ks

    hs :: VP.Vector (Bloom.CheapHashes SerialisedKey)
    !hs  = VP.generate ksN $ \i -> Bloom.makeCheapHashes (V.unsafeIndex ks i)

    -- Loop over all run indexes
    loop1 ::
         VUM.MVector s (RunIx, KeyIx)
      -> ResIx
      -> RunIx
      -> ST s (VUM.MVector s (RunIx, KeyIx))
    loop1 !res1 !resix1 !rix
      | rix == rsN = pure $ VUM.slice 0 resix1 res1
      | otherwise
      = do
          (res1', resix1') <- loop2 res1 resix1 0 (blooms `V.unsafeIndex` rix)
          loop1 res1' resix1' (rix+1)
      where
        -- Loop over all key indexes
        loop2 ::
             VUM.MVector s (RunIx, KeyIx)
          -> ResIx
          -> KeyIx
          -> Bloom SerialisedKey
          -> ST s (VUM.MVector s (RunIx, KeyIx), ResIx)
        loop2 !res2 !resix2 !kix !b
          | kix == ksN = pure (res2, resix2)
          | let !h = hs `VP.unsafeIndex` kix
          , Bloom.elemHashes h b = do
              -- Grows the vector if we've reached the end. TODO: tune how
              -- much much we grow the vector each time based on the
              -- expected true- and false-positives.
              res2' <- if resix2 == VUM.length res2
                        then VUM.unsafeGrow res2 ksN
                        else pure res2
              VUM.unsafeWrite res2' resix2 (rix, kix)
              loop2 res2' (resix2+1) (kix+1) b
          | otherwise = loop2 res2 resix2 (kix+1) b

-- | Perform a batch of fence pointer index searches, and create an 'IOOp' for
-- each search result. The resulting vector has the same length as the
-- @VU.Vector (RunIx, KeyIx)@ argument, because index searching always returns a
-- positive search result.
indexSearches ::
     V.Vector IndexCompact
  -> V.Vector (Handle h)
  -> V.Vector SerialisedKey
  -> VU.Vector (RunIx, KeyIx) -- ^ Result of 'bloomQueries'
  -> ST s (V.Vector (IOOp s h))
indexSearches !indexes !kopsFiles !ks !rkixs = V.generateM n $ \i -> do
    let (!rix, !kix) = rkixs `VU.unsafeIndex` i
        !c           = indexes `V.unsafeIndex` rix
        !h           = kopsFiles `V.unsafeIndex` rix
        !k           = ks `V.unsafeIndex` kix
        !pspan       = Index.search k c
        !size        = Index.pageSpanSize pspan
    -- The current allocation strategy is to allocate a new pinned
    -- byte array for each 'IOOp'. One optimisation we are planning to
    -- do is to use a cache of re-usable buffers, in which case we
    -- decrease the GC load. TODO: re-usable buffers.
    !buf <- newAlignedPinnedByteArray (size * 4096) 4096
    pure $! IOOpRead
              h
              (fromIntegral $ Index.unPageNo (pageSpanStart pspan) * 4096)
              buf
              0
              (fromIntegral $ size * 4096)
  where
    !n = VU.length rkixs

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

-- | Value resolve function: what to do when resolving two @Mupdate@s
type ResolveSerialisedValue = SerialisedValue -> SerialisedValue -> SerialisedValue

-- | An 'IOOp' read/wrote fewer or more bytes than expected
data ByteCountDiscrepancy = ByteCountDiscrepancy {
    expected :: ByteCount
  , actual   :: ByteCount
  }
  deriving (Show, Exception)

{-# SPECIALIZE lookupsIO ::
       HasBlockIO IO h
    -> ResolveSerialisedValue
    -> V.Vector (Run (Handle h))
    -> V.Vector (Bloom SerialisedKey)
    -> V.Vector IndexCompact
    -> V.Vector (Handle h)
    -> V.Vector SerialisedKey
    -> IO (V.Vector (Maybe (Entry SerialisedValue (BlobRef (Run (Handle h))))))
  #-}
-- | Batched lookups in I\/O.
--
-- See Note [Batched lookups, buffer strategy and restrictions]
--
-- TODO: optimise by reducing allocations, possibly looking at core.
lookupsIO ::
     forall m h. (PrimMonad m, MonadThrow m, MonadST m)
  => HasBlockIO m h
  -> ResolveSerialisedValue
  -> V.Vector (Run (Handle h))
  -> V.Vector (Bloom SerialisedKey)
  -> V.Vector IndexCompact
  -> V.Vector (Handle h)
  -> V.Vector SerialisedKey
  -> m (V.Vector (Maybe (Entry SerialisedValue (BlobRef (Run (Handle h))))))
lookupsIO !hbio !resolveV !rs !blooms !indexes !kopsFiles !ks = assert precondition $ do
    (rkixs, ioops) <- Class.stToIO $ prepLookups blooms indexes kopsFiles ks
    ioress <- submitIO hbio ioops
    intraPageLookups resolveV rs ks rkixs ioops ioress
  where
    precondition = and [
          V.map Run.runFilter rs == blooms
        , V.map Run.runIndex rs == indexes
        , V.length rs == V.length kopsFiles
        ]

{-# SPECIALIZE intraPageLookups ::
       ResolveSerialisedValue
    -> V.Vector (Run (Handle h))
    -> V.Vector SerialisedKey
    -> VU.Vector (RunIx, KeyIx)
    -> V.Vector (IOOp RealWorld h)
    -> VU.Vector IOResult
    -> IO (V.Vector (Maybe (Entry SerialisedValue (BlobRef (Run (Handle h))))))
  #-}
-- | Intra-page lookups.
--
-- This function assumes that @rkixs@ is ordered such that newer runs are
-- handled first. The order matters for resolving cases where we find the same
-- key in multiple runs.
--
-- TODO: optimise by reducing allocations, possibly looking at core, using
-- unsafe vector operations.
--
-- PRECONDITION: @length rkixs == length ioops == length ioress@
intraPageLookups ::
     forall m h. (PrimMonad m, MonadThrow m)
  => ResolveSerialisedValue
  -> V.Vector (Run (Handle h))
  -> V.Vector SerialisedKey
  -> VU.Vector (RunIx, KeyIx)
  -> V.Vector (IOOp (PrimState m) h)
  -> VU.Vector IOResult
  -> m (V.Vector (Maybe (Entry SerialisedValue (BlobRef (Run (Handle h))))))
intraPageLookups !resolveV !rs !ks !rkixs !ioops !ioress =
    assert precondition $ do
      res <- VM.replicate (V.length ks) Nothing
      loop res 0
      V.unsafeFreeze res
  where
    precondition = and [
          VU.length rkixs == V.length ioops
        , V.length ioops == VU.length ioress
        ]

    !n = V.length ioops

    loop ::
         VM.MVector (PrimState m) (Maybe (Entry SerialisedValue (BlobRef (Run (Handle h)))))
      -> Int
      -> m ()
    loop !res !ioopix
      | ioopix == n =  pure ()
      | otherwise = do
          let ioop = ioops `V.unsafeIndex` ioopix
              iores = ioress `VU.unsafeIndex` ioopix
          checkIOResult ioop iores
          let (rix, kix) = rkixs `VU.unsafeIndex` ioopix
              r = rs `V.unsafeIndex` rix
              k = ks `V.unsafeIndex` kix
          buf <- unsafeFreezeByteArray (ioopBuffer ioop)
          let boff = unBufferOffset $ ioopBufferOffset ioop
          case rawPageLookup (makeRawPage buf boff) k of
            LookupEntryNotPresent -> pure ()
            -- Laziness ensures that we only compute the forcing of the value in
            -- the entry when the result is needed.
            LookupEntry e         -> do
                let e' = bimap copySerialisedValue (BlobRef r) e
                unsafeInsertWithMStrict res (combine resolveV) kix e'
            -- Laziness ensures that we only compute the appending of the prefix
            -- and suffix when the result is needed. We do not use 'force' here,
            -- since appending already creates a new primary vector.
            --
            -- TODO: verify if appending always computes a new primary vector
            LookupEntryOverflow e m -> do
                let v' (SerialisedValue v) = SerialisedValue $ v <>
                      RawBytes (mkPrimVector
                                  (unBufferOffset (ioopBufferOffset ioop) + 4096)
                                  (fromIntegral m)
                                  buf)
                    e' = bimap v' (BlobRef r) e
                unsafeInsertWithMStrict res (combine resolveV) kix e'
          loop res (ioopix + 1)

    -- Check that the IOOp was performed succesfully, and that it wrote/read
    -- exactly as many bytes as we expected. If not, then the buffer won't
    -- contain the correct number of disk-page bytes, so we throw an exception.
    checkIOResult :: IOOp (PrimState m) h -> IOResult -> m ()
    checkIOResult ioop (IOResult m) =
        unless (expected == m) $
          throwIO ByteCountDiscrepancy {expected, actual = m}
      where expected = ioopByteCount ioop

    -- Force a serialised value to not retain any memory by copying the
    -- underlying raw bytes.
    copySerialisedValue :: SerialisedValue -> SerialisedValue
    copySerialisedValue (SerialisedValue rb) =
        SerialisedValue (RB.copy rb)

{-# INLINE unsafeInsertWithMStrict #-}
-- | Insert (in a broad sense) an entry in a mutable vector at a given index,
-- but if a @Just@ entry already exists at that index, combine the two entries
-- using @f@.
unsafeInsertWithMStrict ::
     PrimMonad m
  => VM.MVector (PrimState m) (Maybe a)
  -> (a -> a -> a)  -- ^ function @f@, called as @f new old@
  -> Int
  -> a
  -> m ()
unsafeInsertWithMStrict mvec f i y = VM.unsafeModifyM mvec g i
  where
    g x = pure $! Just $! maybe y (`f` y) x
