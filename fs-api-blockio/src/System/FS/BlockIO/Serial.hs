{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.FS.BlockIO.Serial (
    serialHasBlockIO
  ) where

import           Control.Concurrent.Class.MonadMVar
import           Control.Monad (unless)
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           System.FS.API
import qualified System.FS.BlockIO.API as API
import           System.FS.BlockIO.API (IOOp (..), IOResult (..))

-- | IO instantiation of 'HasBlockIO', using an existing 'HasFS'. Thus this
-- implementation does not take advantage of parallel I/O.
serialHasBlockIO ::
     (MonadThrow m, MonadMVar m, PrimMonad m, Eq h)
  => (Handle h -> m Bool)
  -> (Handle h -> Bool -> m ())
  -> HasFS m h
  -> API.IOCtxParams
  -> m (API.HasBlockIO m h)
serialHasBlockIO readNoCache writeNoCache hfs ctxParams = do
  ctx <- initIOCtx (SomeHasFS hfs)
  pure $ API.HasBlockIO {
      API.close = close ctx
    , API.submitIO = submitIO hfs ctx
    , API.getParams = ctxParams
    , API.readNoCache
    , API.writeNoCache
    }

data IOCtx m = IOCtx { ctxFS :: SomeHasFS m, openVar :: MVar m Bool }

guardIsOpen :: (MonadMVar m, MonadThrow m) => IOCtx m -> m ()
guardIsOpen ctx = readMVar (openVar ctx) >>= \b ->
    unless b $ throwIO (API.mkClosedError (ctxFS ctx) "submitIO")

initIOCtx :: MonadMVar m => SomeHasFS m -> m (IOCtx m)
initIOCtx someHasFS = IOCtx someHasFS <$> newMVar True

close :: MonadMVar m => IOCtx m -> m ()
close ctx = modifyMVar_ (openVar ctx) $ const (pure False)

submitIO ::
     (MonadMVar m, MonadThrow m, PrimMonad m)
  => HasFS m h
  -> IOCtx m
  -> V.Vector (IOOp (PrimState m) h)
  -> m (VU.Vector IOResult)
submitIO hfs ctx ioops = do
    guardIsOpen ctx
    hmapM (ioop hfs) ioops

-- | Perform the IOOp using synchronous I\/O.
ioop ::
     MonadThrow m
  => HasFS m h
  -> IOOp (PrimState m) h
  -> m IOResult
ioop hfs (IOOpRead h off buf bufOff c) =
    IOResult <$> hGetBufExactlyAt hfs h buf bufOff c (fromIntegral off)
ioop hfs (IOOpWrite h off buf bufOff c) =
    IOResult <$> hPutBufExactlyAt hfs h buf bufOff c (fromIntegral off)

-- | Heterogeneous blend of 'V.mapM' and 'VU.mapM'.
--
-- The @vector@ package does not provide functions that take distinct vector
-- containers as arguments, so we write it by hand to prevent having to convert
-- one vector type to the other.
hmapM ::
     forall m a b. (PrimMonad m, VUM.Unbox b)
  => (a -> m b)
  -> V.Vector a
  -> m (VU.Vector b)
hmapM f v = do
    res <- VUM.unsafeNew n
    loop res 0
  where
    !n = V.length v
    loop !res !i
      | i == n = VU.unsafeFreeze res
      | otherwise = do
          let !x = v `V.unsafeIndex` i
          !z <- f x
          VUM.unsafeWrite res i z
          loop res (i+1)
