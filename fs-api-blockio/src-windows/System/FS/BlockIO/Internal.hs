module System.FS.BlockIO.Internal (
    ioHasBlockIO
  ) where

import           System.FS.API (Handle, HasFS)
import           System.FS.BlockIO.API (HasBlockIO, IOCtxParams)
import qualified System.FS.BlockIO.Serial as Serial
import           System.FS.IO (HandleIO)

-- | For now we use the portable serial implementation of HasBlockIO. If you
-- want to provide a proper async I/O implementation for Windows, then this is
-- where you should put it.
--
-- The recommended choice would be to use the Win32 IOCP API.
ioHasBlockIO ::
     HasFS IO HandleIO
  -> IOCtxParams
  -> IO (HasBlockIO IO HandleIO)
ioHasBlockIO = Serial.serialHasBlockIO readNoCache writeNoCache

readNoCache :: Handle HandleIO -> IO Bool
readNoCache _h = pure False

writeNoCache :: Handle HandleIO -> Bool -> IO ()
writeNoCache _h _b = pure ()
