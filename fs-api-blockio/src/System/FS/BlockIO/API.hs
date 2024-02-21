{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module System.FS.BlockIO.API (
    HasBlockIO (..)
  , IOCtxParams (..)
  , mkClosedError
  , IOOp (..)
  , ioopHandle
  , ioopBufferOffset
  , ioopByteCount
  , IOResult (..)
    -- * Re-exports
  , ByteCount
  , FileOffset
  ) where

import           Control.Monad.Primitive (PrimMonad (PrimState))
import           Data.Primitive.ByteArray (MutableByteArray)
import           GHC.IO.Exception (IOErrorType (ResourceVanished))
import           System.FS.API
import           System.IO.Error (ioeSetErrorString, mkIOError)
import           System.Posix.Types (ByteCount, FileOffset)
import           Util.CallStack

-- | Abstract interface for submitting large batches of I\/O operations.
data HasBlockIO m h = HasBlockIO {
    -- | (Idempotent) close the interface.
    --
    -- Using 'submitIO' after 'close' should thrown an 'FsError' exception. See
    -- 'mkClosedError'.
    close    :: HasCallStack => m ()
    -- | Submit a batch of I\/O operations and wait for the result.
    --
    -- Results correspond to input 'IOOp's in a pair-wise manner, i.e., one can
    -- match 'IOOp's with 'IOResult's by zipping the input and output list.
    --
    -- If any of the I\/O operations fails, an 'FsError' exception will be thrown.
  , submitIO :: HasCallStack => [IOOp m h] -> m [IOResult]
  }

-- | Concurrency parameters for initialising a 'HasBlockIO. Can be ignored by
-- serial implementations.
data IOCtxParams = IOCtxParams {
                     ioctxBatchSizeLimit   :: !Int,
                     ioctxConcurrencyLimit :: !Int
                   }

mkClosedError :: HasCallStack => SomeHasFS m -> String -> FsError
mkClosedError (SomeHasFS hasFS) loc = ioToFsError (mkFsErrorPath hasFS (mkFsPath [])) ioerr
  where ioerr =
          ioeSetErrorString
            (mkIOError ResourceVanished loc Nothing Nothing)
            ("HasBlockIO closed: " <> loc)


data IOOp m h =
    IOOpRead  !(Handle h) !FileOffset !(MutableByteArray (PrimState m)) !BufferOffset !ByteCount
  | IOOpWrite !(Handle h) !FileOffset !(MutableByteArray (PrimState m)) !BufferOffset !ByteCount

ioopHandle :: IOOp m h -> Handle h
ioopHandle (IOOpRead h _ _ _ _)  = h
ioopHandle (IOOpWrite h _ _ _ _) = h

ioopBufferOffset :: IOOp m h -> BufferOffset
ioopBufferOffset (IOOpRead _ _ _ bufOff _)  = bufOff
ioopBufferOffset (IOOpWrite _ _ _ bufOff _) = bufOff

ioopByteCount :: IOOp m h -> ByteCount
ioopByteCount (IOOpRead _ _ _ _ c)  = c
ioopByteCount (IOOpWrite _ _ _ _ c) = c

-- | Number of read/written bytes.
newtype IOResult = IOResult ByteCount
