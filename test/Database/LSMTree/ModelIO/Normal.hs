{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}

-- Model's 'open' and 'snapshot' have redundant constraints.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | IO-based model implementation.
--
-- Differences from the (current) real API:
--
-- * `openSession` doesn't take file-system arguments.
--
-- * `snapshot` and `open` require `Typeable` constraints
--
module Database.LSMTree.ModelIO.Normal (
    -- * Temporary placeholder types
    Model.SomeSerialisationConstraint (..)
    -- * Utility types
  , IOLike
    -- * Sessions
  , Session
  , openSession
  , closeSession
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
  , SnapshotName
  , snapshot
  , open
  , deleteSnapshot
  , listSnapshots
    -- * Multiple writable table handles
  , duplicate
  ) where

import           Control.Concurrent.Class.MonadSTM
import           Control.Monad (void)
import           Data.Dynamic (fromDynamic, toDyn)
import           Data.Kind (Type)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Database.LSMTree.Common (IOLike, SnapshotName,
                     SomeSerialisationConstraint)
import qualified Database.LSMTree.Model.Normal as Model
import           Database.LSMTree.ModelIO.Session
import           Database.LSMTree.Normal (LookupResult (..),
                     RangeLookupResult (..), Update (..))
import           GHC.IO.Exception (IOErrorType (..), IOException (..))
import           System.IO.Error (alreadyExistsErrorType)
import           Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
  Tables
-------------------------------------------------------------------------------}

-- | A handle to a table.
type TableHandle :: (Type -> Type) -> Type -> Type -> Type -> Type
data TableHandle m k v blob = TableHandle {
    thSession :: !(Session m)
  , thId      :: !Int
  , thRef     :: !(TMVar m (Model.Table k v blob))
  }

data TableConfig = TableConfig
  deriving Show

-- | Configs should be comparable, because only tables with the same config
-- options are __compatible__.
deriving instance Eq TableConfig

-- | Create a new table referenced by a table handle.
new ::
     IOLike m
  => Session m
  -> TableConfig
  -> m (TableHandle m k v blob)
new session _config = atomically $ do
    ref <- newTMVar Model.empty
    i <- new_handle session ref
    return TableHandle {thSession = session, thId = i, thRef = ref }

-- | Close a table handle.
close ::
     IOLike m
  => TableHandle m k v blob
  -> m ()
close TableHandle {..} = atomically $ do
    close_handle thSession thId
    void $ tryTakeTMVar thRef

{-------------------------------------------------------------------------------
  Table querying and updates
-------------------------------------------------------------------------------}

-- | Perform a batch of lookups.
lookups ::
     (IOLike m, SomeSerialisationConstraint k, SomeSerialisationConstraint v)
  => [k]
  -> TableHandle m k v blob
  -> m [LookupResult k v (BlobRef m blob)]
lookups ks th@TableHandle {..} = atomically $
    withModel "lookups" thSession thRef $ \tbl ->
        return $ liftBlobRefs th $ Model.lookups ks tbl

-- | Perform a range lookup.
rangeLookup ::
     (IOLike m, SomeSerialisationConstraint k, SomeSerialisationConstraint v)
  => Model.Range k
  -> TableHandle m k v blob
  -> m [RangeLookupResult k v (BlobRef m blob)]
rangeLookup r th@TableHandle {..} = atomically $
    withModel "rangeLookup" thSession thRef $ \tbl ->
        return $ liftBlobRefs th $ Model.rangeLookup r tbl

-- | Perform a mixed batch of inserts and deletes.
updates ::
     ( IOLike m
     , SomeSerialisationConstraint k
     , SomeSerialisationConstraint v
     , SomeSerialisationConstraint blob
     )
  => [(k, Update v blob)]
  -> TableHandle m k v blob
  -> m ()
updates ups TableHandle {..} = atomically $
    withModel "updates" thSession thRef $ \tbl ->
        writeTMVar thRef $ Model.updates ups tbl

-- | Perform a batch of inserts.
inserts ::
     ( IOLike m
     , SomeSerialisationConstraint k
     , SomeSerialisationConstraint v
     , SomeSerialisationConstraint blob
     )
  => [(k, v, Maybe blob)]
  -> TableHandle m k v blob
  -> m ()
inserts = updates . fmap (\(k, v, blob) -> (k, Model.Insert v blob))

-- | Perform a batch of deletes.
deletes ::
     ( IOLike m
     , SomeSerialisationConstraint k
     , SomeSerialisationConstraint v
     , SomeSerialisationConstraint blob
     )
  => [k]
  -> TableHandle m k v blob
  -> m ()
deletes = updates . fmap (,Model.Delete)

data BlobRef m blob = BlobRef {
    brSession :: !(Session m)
  , brRef     :: !(forall k v. TMVar m (Model.Table k v blob))
  , brBlob    :: !(Model.BlobRef blob)
  }

liftBlobRefs ::
     Functor f
  => TableHandle m k v blob
  -> [f (Model.BlobRef blob)]
  -> [f (BlobRef m blob)]
liftBlobRefs TableHandle{..} = fmap (fmap (BlobRef thSession (unsafeCoerce thRef)))

-- | Perform a batch of blob retrievals.
retrieveBlobs ::
     forall m blob. (IOLike m, SomeSerialisationConstraint blob)
  => [BlobRef m blob]
  -> m [blob]
retrieveBlobs brefs = atomically $ Model.retrieveBlobs <$> mapM guard brefs
  where
    -- Ensure that the session and table handle for each of the blob refs are
    -- still open.
    guard :: BlobRef m blob -> STM m (Model.BlobRef blob)
    guard BlobRef{..} =
      withModel "retrieveBlobs" brSession brRef $ \_tbl -> pure brBlob

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

-- | Take a snapshot.
snapshot ::
     ( IOLike m
     , SomeSerialisationConstraint k
     , SomeSerialisationConstraint v
     , SomeSerialisationConstraint blob
     , Typeable k
     , Typeable v
     , Typeable blob
     )
  => SnapshotName
  -> TableHandle m k v blob
  -> m ()
snapshot n TableHandle {..} = atomically $
    withModel "snapshot" thSession thRef $ \tbl -> do
        snaps <- readTVar $ snapshots thSession
        if Map.member n snaps then
          throwSTM IOError
            { ioe_handle      = Nothing
            , ioe_type        = alreadyExistsErrorType
            , ioe_location    = "snapshot"
            , ioe_description = "snapshot already exists"
            , ioe_errno       = Nothing
            , ioe_filename    = Nothing
            }
        else
          modifyTVar' (snapshots thSession) (Map.insert n (toDyn tbl))

-- | Open a table through a snapshot, returning a new table handle.
open ::
     ( IOLike m
     , SomeSerialisationConstraint k
     , SomeSerialisationConstraint v
     , SomeSerialisationConstraint blob
     , Typeable k
     , Typeable v
     , Typeable blob
     )
  => Session m
  -> SnapshotName
  -> m (TableHandle m k v blob)
open s n = atomically $ do
    ss <- readTVar (snapshots s)
    case Map.lookup n ss of
        Nothing -> throwSTM IOError
            { ioe_handle      = Nothing
            , ioe_type        = NoSuchThing
            , ioe_location    = "open"
            , ioe_description = "no such snapshot"
            , ioe_errno       = Nothing
            , ioe_filename    = Nothing
            }

        Just dyn -> case fromDynamic dyn of
            Nothing -> throwSTM IOError
                { ioe_handle      = Nothing
                , ioe_type        = InappropriateType
                , ioe_location    = "open"
                , ioe_description = "table type mismatch"
                , ioe_errno       = Nothing
                , ioe_filename    = Nothing
                }

            Just tbl' -> do
                ref <- newTMVar tbl'
                i <- new_handle s ref
                return TableHandle { thRef = ref, thId = i, thSession = s }

{-------------------------------------------------------------------------------
  Mutiple writable table handles
-------------------------------------------------------------------------------}

-- | Create a cheap, independent duplicate of a table handle. This returns a new
-- table handle.
duplicate ::
     IOLike m
  => TableHandle m k v blob
  -> m (TableHandle m k v blob)
duplicate TableHandle {..} = atomically $
    withModel "duplicate" thSession thRef $ \tbl -> do
        thRef' <- newTMVar tbl
        i <- new_handle thSession thRef'
        return TableHandle { thRef = thRef', thId = i, thSession = thSession }

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

withModel :: IOLike m => String -> Session m -> TMVar m a -> (a -> STM m r) -> STM m r
withModel fun s ref kont = do
    m <- tryReadTMVar ref
    case m of
        Nothing -> throwSTM IOError
            { ioe_handle      = Nothing
            , ioe_type        = IllegalOperation
            , ioe_location    = fun
            , ioe_description = "table handle closed"
            , ioe_errno       = Nothing
            , ioe_filename    = Nothing
            }
        Just m' -> do
            check_session_open fun s
            kont m'

writeTMVar :: MonadSTM m => TMVar m a -> a -> STM m ()
writeTMVar t n = tryTakeTMVar t >> putTMVar t n
