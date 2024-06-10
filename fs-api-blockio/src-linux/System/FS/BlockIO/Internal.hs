{-# LANGUAGE CPP #-}

module System.FS.BlockIO.Internal (
    ioHasBlockIO
  ) where

import           System.FS.API (Handle (handleRaw), HasFS)
import           System.FS.BlockIO.API (HasBlockIO, IOCtxParams)
#if SERIALBLOCKIO
import qualified System.FS.BlockIO.Serial as Serial
#else
import qualified System.FS.BlockIO.Async as Async
#endif
import           System.FS.IO (HandleIO)
import           System.FS.IO.Handle (withOpenHandle)
import qualified System.Posix.Fcntl.NoCache as Unix

ioHasBlockIO ::
     HasFS IO HandleIO
  -> IOCtxParams
  -> IO (HasBlockIO IO HandleIO)
#if SERIALBLOCKIO
ioHasBlockIO = Serial.serialHasBlockIO
#else
ioHasBlockIO = Async.asyncHasBlockIO readNoCache writeNoCache
#endif

readNoCache :: Handle HandleIO -> IO Bool
readNoCache h = withOpenHandle "readNoCache" (handleRaw h) Unix.readFcntlNoCache

writeNoCache :: Handle HandleIO -> Bool -> IO ()
writeNoCache h b =
  withOpenHandle "writeNoCache" (handleRaw h) (flip Unix.writeFcntlNoCache b)
