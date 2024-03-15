{-# LANGUAGE RecordWildCards #-}

-- | Datastructures and algorithms for k-way merges.
--
-- === TODO
--
-- This is temporary module header documentation. The module will be
-- fleshed out more as we implement bits of it.
--
-- Related work packages: 5, 6
--
-- This module includes in-memory parts for, amongst others,
--
-- * k-way merge data structures (heap or tournament tree)
--
-- * k-way merge algorithms
--
-- The above list is a sketch. Functionality may move around, and the list is
-- not exhaustive.
--
module Database.LSMTree.Internal.KMerge (
    Merge (..)
  , new
  , step
  ) where

import           Control.Monad.Primitive (RealWorld)
import           Data.Function (on)
import           Data.IORef
import           Data.Maybe (fromJust)  -- TODO
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.Run (Run)
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.Run.Mutable (MRun)
import qualified Database.LSMTree.Internal.Run.Mutable as MRun
import           Database.LSMTree.Internal.Run.Reader (Reader)
import qualified Database.LSMTree.Internal.Run.Reader as Reader
import           Database.LSMTree.Internal.Serialise
import qualified KMerge.Heap as Heap
import qualified System.FS.API as FS
import           System.FS.API (HasFS)

data Merge fhandle = Merge {
      mergeInputs    :: !(Heap.MutableHeap RealWorld (Input fhandle))
    , mergeNextInput :: !(IORef (Input fhandle))
    , mergeOutput    :: !(MRun fhandle)
    }

-- Input ----------------------------------------------------------------------

data Input fhandle = Input {
      inputNextKey   :: !SerialisedKey
    , inputNextEntry :: !(Entry SerialisedValue SerialisedBlob)
    , inputReader    :: !(Reader fhandle)
    }

instance Eq (Input fhandle) where
  (==) = (==) `on` inputNextKey

instance Ord (Input fhandle) where
  compare = compare `on` inputNextKey

makeInput :: HasFS IO h -> Run (FS.Handle h) -> IO (Input (FS.Handle h))
makeInput fs run = do
    inputReader <- Reader.new fs run
    (inputNextKey, inputNextEntry) <- fromJust <$> Reader.readNext fs inputReader
    return Input {..}

nextInput ::
     HasFS IO h
  -> Input (FS.Handle h)
  -> IO (Maybe (Input (FS.Handle h)))
nextInput fs Input {..} =
    fmap (\(key, entry) -> Input key entry inputReader)
      <$> Reader.readNext fs inputReader

-------------------------------------------------------------------------------

new :: HasFS IO h -> [Run (FS.Handle h)] -> IO (Merge (FS.Handle h))
new fs inputs = do
    (mergeInputs, firstInput) <-
      Heap.newMutableHeap =<< traverse (makeInput fs) inputs
    mergeNextInput <- newIORef (fromJust firstInput)  -- TODO

    let outputPaths = undefined
    let numEntries = undefined
    let estimatedNumPages = undefined
    mergeOutput <- MRun.new fs outputPaths numEntries estimatedNumPages

    return Merge {..}

-- | Produce a single entry (which could use multiple inputs with the same key).
step :: HasFS IO h -> Merge (FS.Handle h) -> IO ()
step fs Merge {..} = do
    Input key entry reader <- readIORef mergeNextInput
    -- consume any inputs that might still be for the same key, resolve
    (resolvedEntry, nextInput) <- undefined
    -- write one entry
    -- TODO: larger than page
    MRun.addFullKOp fs mergeOutput key resolvedEntry
    -- remember next key and entry
    writeIORef mergeNextInput nextInput
    return ()
  where
    getNextInput :: SerialisedKey -> Entry SerialisedKey SerialisedBlob
      -> (Entry SerialisedKey SerialisedBlob, Input (FS.Handle h))
    getNextInput = undefined
