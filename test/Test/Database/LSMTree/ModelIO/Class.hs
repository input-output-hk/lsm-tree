{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
module Test.Database.LSMTree.ModelIO.Class (
    IsSession (..)
  , IsTableHandle (..)
  ) where

import           Control.Monad.Class.MonadThrow (MonadThrow (throwIO))
import           Data.Kind (Constraint, Type)
import           Data.Proxy (Proxy)
import           Data.Typeable (Typeable)
import           Database.LSMTree.Common (IOLike, Range (..), SnapshotName,
                     SomeSerialisationConstraint)
import qualified Database.LSMTree.ModelIO.Normal as M
import           Database.LSMTree.Normal (LookupResult (..),
                     RangeLookupResult (..), Update (..))
import qualified Database.LSMTree.Normal as R

type IsSession :: ((Type -> Type) -> Type) -> Constraint
class IsSession s where
    openSession :: IOLike m => m (s m)

    closeSession :: IOLike m => s m -> m ()

    deleteSnapshot ::
           IOLike m
        => s m
        -> SnapshotName
        -> m ()

    listSnapshots ::
           IOLike m
        => s m
        -> m [SnapshotName]

-- | Class abstracting over table handle operations.
--
type IsTableHandle :: ((Type -> Type) -> Type -> Type -> Type -> Type) -> Constraint
class (IsSession (Session h)) => IsTableHandle h where
    type Session h :: (Type -> Type) -> Type
    type TableConfig h :: Type
    type BlobRef h :: (Type -> Type) -> Type -> Type

    testTableConfig :: Proxy h -> TableConfig h

    new ::
           IOLike m
        => Session h m
        -> TableConfig h
        -> m (h m k v blob)

    close ::
           IOLike m
        => h m k v blob
        -> m ()

    lookups ::
            (IOLike m, SomeSerialisationConstraint k, SomeSerialisationConstraint v)
        => h m k v blob
        -> [k]
        -> m [LookupResult k v (BlobRef h m blob)]

    rangeLookup ::
            (IOLike m, SomeSerialisationConstraint k, SomeSerialisationConstraint v)
        => h m k v blob
        -> Range k
        -> m [RangeLookupResult k v (BlobRef h m blob)]

    retrieveBlobs ::
            (IOLike m, SomeSerialisationConstraint blob)
        => proxy h
        -> [BlobRef h m blob]
        -> m [blob]

    updates ::
        ( IOLike m
        , SomeSerialisationConstraint k
        , SomeSerialisationConstraint v
        , SomeSerialisationConstraint blob
        )
        => h m k v blob
        -> [(k, Update v blob)]
        -> m ()

    inserts ::
        ( IOLike m
        , SomeSerialisationConstraint k
        , SomeSerialisationConstraint v
        , SomeSerialisationConstraint blob
        )
        => h m k v blob
        -> [(k, v, Maybe blob)]
        -> m ()

    deletes ::
        ( IOLike m
        , SomeSerialisationConstraint k
        , SomeSerialisationConstraint v
        , SomeSerialisationConstraint blob
        )
        => h m k v blob
        -> [k]
        -> m ()

    snapshot ::
        ( IOLike m
        , SomeSerialisationConstraint k
        , SomeSerialisationConstraint v
        , SomeSerialisationConstraint blob
          -- Model-specific constraints
        , Typeable k
        , Typeable v
        , Typeable blob
        )
        => SnapshotName
        -> h m k v blob
        -> m ()

    open ::
        ( IOLike m
        , SomeSerialisationConstraint k
        , SomeSerialisationConstraint v
        , SomeSerialisationConstraint blob
          -- Model-specific constraints
        , Typeable k
        , Typeable v
        , Typeable blob
        )
        => Session h m
        -> SnapshotName
        -> m (h m k v blob)

    duplicate ::
            IOLike m
        => h m k v blob
        -> m (h m k v blob)

instance IsSession M.Session where
    openSession = M.openSession
    closeSession = M.closeSession
    deleteSnapshot = M.deleteSnapshot
    listSnapshots = M.listSnapshots

instance IsTableHandle M.TableHandle where
    type Session M.TableHandle = M.Session
    type TableConfig M.TableHandle = M.TableConfig
    type BlobRef M.TableHandle = M.BlobRef

    testTableConfig _ = M.TableConfig

    new = M.new
    close = M.close
    lookups = flip M.lookups
    updates = flip M.updates
    inserts = flip M.inserts
    deletes = flip M.deletes

    rangeLookup = flip M.rangeLookup
    retrieveBlobs _ = M.retrieveBlobs

    snapshot = M.snapshot
    open = M.open

    duplicate = M.duplicate

instance IsSession R.Session where
    openSession = throwIO (userError "openSession unimplemented")
    closeSession = R.closeSession
    deleteSnapshot = R.deleteSnapshot
    listSnapshots = R.listSnapshots

instance IsTableHandle R.TableHandle where
    type Session R.TableHandle = R.Session
    type TableConfig R.TableHandle = R.TableConfig
    type BlobRef R.TableHandle = R.BlobRef

    testTableConfig _ = error "TODO: test TableConfig"

    new = R.new
    close = R.close
    lookups = flip R.lookups
    updates = flip R.updates
    inserts = flip R.inserts
    deletes = flip R.deletes

    rangeLookup = flip R.rangeLookup
    retrieveBlobs _ = R.retrieveBlobs

    snapshot = R.snapshot
    open = R.open

    duplicate = R.duplicate
