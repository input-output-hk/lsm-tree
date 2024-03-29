
module System.FS.BlockIO.Serial (
    serialHasBlockIO
  ) where

import           Control.Concurrent.Class.MonadMVar
import           Control.Monad (unless)
import           Control.Monad.Class.MonadThrow
import           System.FS.API
import qualified System.FS.BlockIO.API as API
import           System.FS.BlockIO.API (IOOp (..), IOResult (..))

-- | IO instantiation of 'HasBlockIO', using an existing 'HasFS'. Thus this
-- implementation does not take advantage of parallel I/O.
serialHasBlockIO ::
     (MonadThrow m, MonadMVar m, Eq h)
  => HasFS m h
  -> HasBufFS m h
  -> m (API.HasBlockIO m h)
serialHasBlockIO hfs hbfs = do
  ctx <- initIOCtx (SomeHasFS hfs)
  pure $ API.HasBlockIO {
      API.close = close ctx
    , API.submitIO = submitIO hfs hbfs ctx
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
     (MonadMVar m, MonadThrow m)
  => HasFS m h
  -> HasBufFS m h
  -> IOCtx m
  -> [IOOp m h]
  -> m [IOResult]
submitIO hfs hbfs ctx ioops = do
  guardIsOpen ctx
  mapM (ioop hfs hbfs) ioops

-- | Perform the IOOp using synchronous I\/O.
ioop ::
     MonadThrow m
  => HasFS m h
  -> HasBufFS m h
  -> IOOp m h
  -> m IOResult
ioop hfs hbfs (IOOpRead h off buf bufOff c) =
    IOResult <$> hGetBufExactlyAt hfs hbfs h buf bufOff c (fromIntegral off)
ioop _hfs hbfs (IOOpWrite h off buf bufOff c) =
    IOResult <$> hPutBufExactlyAt hbfs h buf bufOff c (fromIntegral off)
