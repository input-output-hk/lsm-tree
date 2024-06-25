module System.FS.BlockIO.Internal (
    ioHasBlockIO
  ) where

import           System.FS.API (Handle (handleRaw), HasFS)
import           System.FS.BlockIO.API (HasBlockIO, IOCtxParams)
import qualified System.FS.BlockIO.Serial as Serial
import           System.FS.IO (HandleIO)
import           System.FS.IO.Handle (withOpenHandle)
import qualified System.Posix.Fcntl.NoCache as Unix

-- | For now we use the portable serial implementation of HasBlockIO. If you
-- want to provide a proper async I/O implementation for OSX, then this is where
-- you should put it.
--
-- The recommended choice would be to use the POSIX AIO API.
ioHasBlockIO ::
     HasFS IO HandleIO
  -> IOCtxParams
  -> IO (HasBlockIO IO HandleIO)
ioHasBlockIO = Serial.serialHasBlockIO readNoCache writeNoCache

readNoCache :: Handle HandleIO -> IO Bool
readNoCache h = withOpenHandle "readNoCache" (handleRaw h) Unix.readFcntlNoCache

writeNoCache :: Handle HandleIO -> Bool -> IO ()
writeNoCache h b =
  withOpenHandle "writeNoCache" (handleRaw h) (flip Unix.writeFcntlNoCache b)
