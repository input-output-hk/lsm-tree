{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- | A run that is being read incrementally
--
module Database.LSMTree.Internal.Run.Reader (
    Reader
  , new
  , readNext
  -- , close
  ) where

import           Control.Monad (when)
import           Control.Monad.Primitive (RealWorld)
import qualified Control.Monad.ST as ST
import qualified Data.ByteString.Builder as BSB
import           Data.Foldable (for_, traverse_)
import           Data.IORef
import           Data.Primitive.PrimVar
import           Data.Traversable (for)
import           Data.Word (Word64)
import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import           Database.LSMTree.Internal.BloomFilter (bloomFilterToBuilder)
import           Database.LSMTree.Internal.CRC32C (CRC32C)
import qualified Database.LSMTree.Internal.CRC32C as CRC
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.RawPage
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.Run.BloomFilter (Bloom)
import           Database.LSMTree.Internal.Run.Construction (RunAcc)
import qualified Database.LSMTree.Internal.Run.Construction as Cons
import           Database.LSMTree.Internal.Run.FsPaths
import           Database.LSMTree.Internal.Run.Index.Compact (CompactIndex,
                     NumPages)
import qualified Database.LSMTree.Internal.Run.Index.Compact as Index
import           Database.LSMTree.Internal.Serialise
import qualified System.FS.API as FS
import           System.FS.API (HasFS)


-- Reader ---------------------------------------------------------------------

-- | Keeps open a reference to the run, remember to close!
data Reader fhandle = Reader {
      readerCurrentPage :: !(IORef RawPage)
    , readerCurrentEntry :: !(PrimVar RealWorld Int)
    , readerKOpsHandle :: !fhandle
    , readerBlobHandle :: !fhandle
    }

new ::
     HasFS IO h
  -> Run.Run (FS.Handle h)
  -> IO (Reader (FS.Handle h))
new fs run = do
    -- increase ref counter
    -- load first page from disk
    undefined

readNext ::
     HasFS IO h
  -> Reader (FS.Handle h)
  -> IO (Maybe (SerialisedKey, Entry SerialisedValue SerialisedBlob))
readNext fs Reader {..} = do
    -- take entry from current page (resolve blob if necessary)
    -- if it was the last one, load a new page from disk
    -- if there are no more pages, close and return Nothing
    undefined
