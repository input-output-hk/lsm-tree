{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

-- | Model a single session, allowing multiple tables.
--
-- Builds on top of "Database.LSMTree.Model.Normal", adding table handle and
-- snapshot administration.
module Database.LSMTree.Model.Normal.Session (
    -- * Model
    Model (..)
  , initModel
    -- ** Constraints
  , C
  , C_
  , Model.SomeSerialisationConstraint (..)
    -- ** ModelT and ModelM
  , ModelT (..)
  , runModelT
  , ModelM
  , runModelM
    -- ** Errors
  , Err (..)
    -- * Tables
  , TableHandle
  , TableConfig (..)
  , new
  , close
    -- * Table querying and updates
    -- ** Queries
  , Model.Range (..)
  , Model.LookupResult (..)
  , lookups
  , Model.RangeLookupResult (..)
  , rangeLookup
    -- ** Updates
  , Model.Update (..)
  , updates
  , inserts
  , deletes
    -- ** Blobs
  , BlobRef
  , retrieveBlobs
    -- * Snapshots
  , SUT.SnapshotName
  , snapshot
  , open
  , deleteSnapshot
  , listSnapshots
    -- * Multiple writable table handles
  , duplicate
  ) where

import           Control.Monad (when)
import           Control.Monad.Except (ExceptT (..), MonadError (..),
                     runExceptT)
import           Control.Monad.Identity (Identity (runIdentity))
import           Control.Monad.State.Strict (MonadState (..), StateT (..), gets,
                     modify)
import           Data.Data (Typeable, cast)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Database.LSMTree.Model.Normal as Model
import qualified Database.LSMTree.Normal as SUT
import           Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

data Model = Model {
    tableHandles      :: Map TableHandleID SomeTable
  , nextTableHandleID :: TableHandleID
  , snapshots         :: Map SUT.SnapshotName Snapshot
  }
  deriving Show

initModel :: Model
initModel = Model {
      tableHandles = Map.empty
    , nextTableHandleID = 0
    , snapshots = Map.empty
    }

data SomeTable where
  SomeTable :: forall k v blob. C k v blob
            => Model.Table k v blob -> SomeTable

instance Show SomeTable where
  show (SomeTable table) = show table

--
-- Constraints
--

type C_ a = (Show a, Eq a, Typeable a)

-- | Common constraints for keys, values and blobs
type C k v blob = (C_ k, C_ v, C_ blob)

--
-- ModelT and ModelM
--

newtype ModelT m a = ModelT { _runModelT :: ExceptT Err (StateT Model m) a }
  deriving stock Functor
  deriving newtype ( Applicative
                   , Monad
                   , MonadState Model
                   , MonadError Err
                   )

runModelT :: ModelT m a -> Model -> m (Either Err a, Model)
runModelT = runStateT . runExceptT . _runModelT

type ModelM = ModelT Identity

runModelM :: ModelM a -> Model -> (Either Err a, Model)
runModelM m = runIdentity . runModelT m

--
-- Errors
--

data Err =
    ErrTableHandleClosed
  | ErrSnapshotExists
  | ErrSnapshotDoesNotExist
  | ErrSnapshotWrongType
  deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Tables
-------------------------------------------------------------------------------}

type TableHandleID = Int

--
-- API
--

data TableHandle k v blob = TableHandle {
    tableHandleID :: TableHandleID
  , config        :: TableConfig
  }
  deriving Show

data TableConfig = TableConfig
  deriving Show

new ::
     forall k v blob m. (MonadState Model m, C k v blob)
  => TableConfig
  -> m (TableHandle k v blob)
new config = state $ \Model{..} ->
    let tableHandle = TableHandle {
            tableHandleID = nextTableHandleID
          , ..
          }
        someTable = SomeTable $ Model.empty @k @v @blob
        tableHandles' = Map.insert nextTableHandleID someTable tableHandles
        nextTableHandleID' = nextTableHandleID + 1
        model' = Model {
            tableHandles = tableHandles'
          , nextTableHandleID = nextTableHandleID'
          , ..
          }
    in  (tableHandle, model')

-- |
--
-- This is idempotent.
close :: MonadState Model m => TableHandle k v blob -> m ()
close TableHandle{..} = state $ \Model{..} ->
    let tableHandles' = Map.delete tableHandleID tableHandles
        model' = Model {
            tableHandles = tableHandles'
          , nextTableHandleID
          , ..
          }
    in ((), model')

--
-- Utility
--

guardTableHandleIsOpen ::
     forall k v blob m. (MonadState Model m, MonadError Err m)
  => TableHandle k v blob
  -> m (Model.Table k v blob)
guardTableHandleIsOpen TableHandle{..} =
    gets (Map.lookup tableHandleID . tableHandles) >>= \case
      Nothing ->
        throwError ErrTableHandleClosed
      Just (SomeTable table) ->
        pure $ unsafeCoerce table

newTableWith ::
     (MonadState Model m, C k v blob)
  => TableConfig
  -> Model.Table k v blob
  -> m (TableHandle k v blob)
newTableWith config tbl = state $ \Model{..} ->
  let tableHandle = TableHandle {
          tableHandleID = nextTableHandleID
        , config
        }
      someTable = SomeTable tbl
      tableHandles' = Map.insert nextTableHandleID someTable tableHandles
      nextTableHandleID' = nextTableHandleID + 1
      model' = Model {
          tableHandles = tableHandles'
        , nextTableHandleID = nextTableHandleID'
        , ..
        }
  in  (tableHandle, model')

{-------------------------------------------------------------------------------
  Table querying and updates
-------------------------------------------------------------------------------}

--
-- API
--

lookups ::
     ( MonadState Model m
     , MonadError Err m
     , Model.SomeSerialisationConstraint k
     , Model.SomeSerialisationConstraint v
     )
  => [k]
  -> TableHandle k v blob
  -> m [Model.LookupResult k v (BlobRef blob)]
lookups ks th = do
    table <- guardTableHandleIsOpen th
    pure $ liftBlobRefs th $ Model.lookups ks table

type RangeLookupResult k v blobref = Model.RangeLookupResult k v blobref

rangeLookup ::
     ( MonadState Model m
     , MonadError Err m
     , Model.SomeSerialisationConstraint k
     , Model.SomeSerialisationConstraint v
     )
  => Model.Range k
  -> TableHandle k v blob
  -> m [RangeLookupResult k v (BlobRef blob)]
rangeLookup r th = do
    table <- guardTableHandleIsOpen th
    pure $ liftBlobRefs th $ Model.rangeLookup r table

updates ::
     ( MonadState Model m
     , MonadError Err m
     , Model.SomeSerialisationConstraint k
     , Model.SomeSerialisationConstraint v
     , Model.SomeSerialisationConstraint blob
     , C k v blob
     )
  => [(k, Model.Update v blob)]
  -> TableHandle k v blob
  -> m ()
updates ups th@TableHandle{..} = do
  table <- guardTableHandleIsOpen th
  let table' = Model.updates ups table
  modify (\m -> m {
      tableHandles = Map.insert tableHandleID (SomeTable table') (tableHandles m)
    })

inserts ::
     ( MonadState Model m
     , MonadError Err m
     , Model.SomeSerialisationConstraint k
     , Model.SomeSerialisationConstraint v
     , Model.SomeSerialisationConstraint blob
     , C k v blob
     )
  => [(k, v, Maybe blob)]
  -> TableHandle k v blob
  -> m ()
inserts = updates . fmap (\(k, v, blob) -> (k, Model.Insert v blob))

deletes ::
     ( MonadState Model m
     , MonadError Err m
     , Model.SomeSerialisationConstraint k
     , Model.SomeSerialisationConstraint v
     , Model.SomeSerialisationConstraint blob
     , C k v blob
     )
  => [k]
  -> TableHandle k v blob
  -> m ()
deletes = updates . fmap (,Model.Delete)

data BlobRef blob = BlobRef {
    parentTable :: forall k v. TableHandle k v blob
  , innerBlob   :: Model.BlobRef blob
  }

deriving instance Show blob => Show (BlobRef blob)

retrieveBlobs ::
     ( MonadState Model m
     , MonadError Err m
     , Model.SomeSerialisationConstraint blob
     )
  => [BlobRef blob]
  -> m [blob]
retrieveBlobs refs = Model.retrieveBlobs <$> mapM guard refs
  where guard BlobRef{..} = guardTableHandleIsOpen parentTable >> pure innerBlob

--
-- Utility
--

liftBlobRefs ::
     Functor f
  => TableHandle k v blob
  -> [f (Model.BlobRef blob)]
  -> [f (BlobRef blob)]
liftBlobRefs th = fmap (fmap (BlobRef (unsafeCoerce th)))

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

data Snapshot = Snapshot TableConfig SomeTable
  deriving Show

--
-- API
--

snapshot ::
     ( MonadState Model m
     , MonadError Err m
     , C k v blob
     )
  => SUT.SnapshotName
  -> TableHandle k v blob
  -> m ()
snapshot name th@TableHandle{..} = do
    table <- guardTableHandleIsOpen th
    snaps <- gets snapshots
    when (Map.member name snaps) $
      throwError ErrSnapshotExists
    modify (\m -> m {
        snapshots = Map.insert name (Snapshot config $ SomeTable $ Model.snapshot table) (snapshots m)
      })

open ::
     forall k v blob m.(
       MonadState Model m
     , MonadError Err m
     , C k v blob
     )
  => SUT.SnapshotName
  -> m (TableHandle k v blob)
open name = do
    snaps <- gets snapshots
    case Map.lookup name snaps of
      Nothing ->
        throwError ErrSnapshotDoesNotExist
      Just (Snapshot conf (SomeTable (table :: Model.Table k' v' blob'))) ->
        case cast @(Model.Table k' v' blob') @(Model.Table k v blob) table of
          Nothing ->
            throwError ErrSnapshotWrongType
          Just table' ->
            newTableWith conf table'

deleteSnapshot ::
     (MonadState Model m, MonadError Err m)
  => SUT.SnapshotName
  -> m ()
deleteSnapshot name = do
    snaps <- gets snapshots
    case Map.lookup name snaps of
      Nothing ->
        throwError ErrSnapshotDoesNotExist
      Just _ ->
        modify (\m -> m {
            snapshots = Map.delete name snaps
          })

listSnapshots ::
     MonadState Model m
  => m [SUT.SnapshotName]
listSnapshots = gets (Map.keys . snapshots)

{-------------------------------------------------------------------------------
  Mutiple writable table handles
-------------------------------------------------------------------------------}

--
-- API
--

duplicate ::
     ( MonadState Model m
     , MonadError Err m
     , C k v blob
     )
  => TableHandle k v blob
  -> m (TableHandle k v blob)
duplicate th@TableHandle{..} = do
    table <- guardTableHandleIsOpen th
    newTableWith config $ Model.duplicate table
