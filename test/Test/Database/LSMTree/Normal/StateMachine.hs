{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE EmptyDataDeriving        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

{-
  TODO: improve generation and shrinking of dependencies. See
  https://github.com/input-output-hk/lsm-tree/pull/4#discussion_r1334295154.

  TODO: The 'RetrieveBlobs' action will currently only retrieve blob references
  that come from a single batch lookup or range lookup. It would be interesting
  to also retrieve blob references from a mix of different batch lookups and/or
  range lookups. This would require some non-trivial changes, such as changes to
  'Op' to also include expressions for manipulating lists, such that we can map
  @'Var' ['SUT.BlobRef' blob]@ to @'Var' ('SUT.BlobRef' blob)@. 'RetrieveBlobs'
  would then hold a list of variables (e.g., @['Var' ('SUT.BlobRef blob')]@)
  instead of a variable of a list (@'Var' ['SUT.BlobRef' blob]@).

  TODO: it is currently not correctly modelled what happens if blob references
  are retrieved from an incorrect table handle.

  TODO: run lockstep tests with IOSim too
-}
module Test.Database.LSMTree.Normal.StateMachine (tests) where

import           Control.Monad ((<=<))
import           Control.Monad.Class.MonadThrow (Handler (..), MonadCatch (..),
                     MonadThrow (..))
import           Control.Monad.IOSim (IOSim)
import           Control.Monad.Reader (ReaderT (..))
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString as BS
import           Data.Constraint (Dict (..))
import           Data.Kind (Type)
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Proxy (..), Typeable, cast)
import           Data.Word (Word64)
import qualified Database.LSMTree.Common as SUT (SomeSerialisationConstraint,
                     mkSnapshotName)
import qualified Database.LSMTree.Model.Normal.Session as Model
import qualified Database.LSMTree.ModelIO.Normal as Impl.ModelIO
import qualified Database.LSMTree.Normal as Impl.Real
import qualified Database.LSMTree.Normal as SUT (LookupResult (..), Range (..),
                     RangeLookupResult (..), SnapshotName, Update (..))
import           GHC.IO.Exception (IOErrorType (..), IOException (..))
import           System.Directory (removeDirectoryRecursive)
import           System.FS.API (MountPoint (..), SomeHasFS (..), mkFsPath)
import           System.FS.IO (ioHasFS)
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.STM (simHasFS')
import           System.IO.Error
import           System.IO.Temp (createTempDirectory,
                     getCanonicalTemporaryDirectory)
import qualified Test.Database.LSMTree.ModelIO.Class as SUT.Class
import           Test.Database.LSMTree.Normal.StateMachine.Op
                     (HasBlobRef (getBlobRef), Op (..))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Arbitrary, Gen)
import           Test.QuickCheck.StateModel hiding (Var)
import           Test.QuickCheck.StateModel.Lockstep
import qualified Test.QuickCheck.StateModel.Lockstep.Defaults as Lockstep.Defaults
import qualified Test.QuickCheck.StateModel.Lockstep.Run as Lockstep.Run
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.Orphans ()
import           Test.Util.TypeFamilyWrappers (WrapBlob (..), WrapBlobRef (..),
                     WrapSession (..), WrapTableHandle (..))

{-------------------------------------------------------------------------------
  Test tree
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Normal.StateMachine" [
      testCase "labelledExamples" $
        QC.labelledExamples $ Lockstep.Run.tagActions (Proxy @(ModelState Impl.Real.TableHandle))
    , propLockstepIO_ModelIOImpl
    , propLockstepIO_RealImpl_RealFS
    , propLockstepIO_RealImpl_MockFS
    ]

propLockstepIO_ModelIOImpl :: TestTree
propLockstepIO_ModelIOImpl = testProperty "propLockstepIO_ModelIOImpl" $
    Lockstep.Run.runActionsBracket
      (Proxy @(ModelState Impl.ModelIO.TableHandle))
      acquire
      release
      (\r session -> runReaderT r (session, handler))
  where
    acquire :: IO (WrapSession Impl.ModelIO.TableHandle IO)
    acquire = WrapSession <$> Impl.ModelIO.openSession

    release :: WrapSession Impl.ModelIO.TableHandle IO -> IO ()
    release (WrapSession session) = Impl.ModelIO.closeSession session

    handler :: Handler IO (Maybe Model.Err)
    handler = Handler $ pure . handler'
      where
        handler' :: IOError -> Maybe Model.Err
        handler' err
          | isDoesNotExistError err
          , ioeGetLocation err == "open"
          = Just Model.ErrSnapshotDoesNotExist

          | isIllegalOperation err
          , ioe_description err == "table handle closed"
          = Just Model.ErrTableHandleClosed

          | isAlreadyExistsError err
          , ioe_location err == "snapshot"
          = Just Model.ErrSnapshotExists

          | ioeGetErrorType err == InappropriateType
          , ioe_location err == "open"
          = Just Model.ErrSnapshotWrongType

          | isDoesNotExistError err
          , ioe_location err == "deleteSnapshot"
          = Just Model.ErrSnapshotDoesNotExist

          | otherwise
          = Nothing

propLockstepIO_RealImpl_RealFS :: TestTree
propLockstepIO_RealImpl_RealFS = testProperty "propLockstepIO_RealImpl_RealFS" $
   QC.expectFailure $ -- TODO: remove once we have a real implementation
    Lockstep.Run.runActionsBracket
      (Proxy @(ModelState Impl.Real.TableHandle))
      acquire
      release
      (\r (_, session) -> runReaderT r (session, handler))
  where
    acquire :: IO (FilePath, WrapSession Impl.Real.TableHandle IO)
    acquire = do
        (tmpDir, someHasFS) <- createSystemTempDirectory "propLockstepIO_RealIO"
        session <- Impl.Real.openSession someHasFS (mkFsPath [])
        pure (tmpDir, WrapSession session)

    release :: (FilePath, WrapSession Impl.Real.TableHandle IO) -> IO ()
    release (tmpDir, WrapSession session) = do
        Impl.Real.closeSession session
        removeDirectoryRecursive tmpDir

    handler :: Handler IO (Maybe Model.Err)
    handler = Handler $ pure . handler'
      where
        handler' :: IOError -> Maybe Model.Err
        handler' _err = Nothing

propLockstepIO_RealImpl_MockFS :: TestTree
propLockstepIO_RealImpl_MockFS = testProperty "propLockstepIO_RealImpl_MockFS" $
   QC.expectFailure $ -- TODO: remove once we have a real implementation
    Lockstep.Run.runActionsBracket
      (Proxy @(ModelState Impl.Real.TableHandle))
      acquire
      release
      (\r session -> runReaderT r (session, handler))
  where
    acquire :: IO (WrapSession Impl.Real.TableHandle IO)
    acquire = do
        someHasFS <- SomeHasFS <$> simHasFS' MockFS.empty
        WrapSession <$> Impl.Real.openSession someHasFS (mkFsPath [])

    release :: WrapSession Impl.Real.TableHandle IO -> IO ()
    release (WrapSession session) = Impl.Real.closeSession session

    handler :: Handler IO (Maybe Model.Err)
    handler = Handler $ pure . handler'
      where
        handler' :: IOError -> Maybe Model.Err
        handler' _err = Nothing

createSystemTempDirectory ::  [Char] -> IO (FilePath, SomeHasFS IO)
createSystemTempDirectory prefix = do
    systemTempDir <- getCanonicalTemporaryDirectory
    tempDir <- createTempDirectory systemTempDir prefix
    pure (tempDir, SomeHasFS $ ioHasFS (MountPoint tempDir))

instance Arbitrary Impl.ModelIO.TableConfig where
  arbitrary :: Gen Impl.ModelIO.TableConfig
  arbitrary = pure Impl.ModelIO.TableConfig

-- TODO: improve, more types of configs
instance Arbitrary Impl.Real.TableConfig where
  arbitrary :: Gen Impl.Real.TableConfig
  arbitrary = pure $  Impl.Real.TableConfig {
        Impl.Real.tcMaxBufferMemory = 2 * 1024 * 1024
      , Impl.Real.tcMaxBloomFilterMemory = 2 * 1024 * 1024
      , Impl.Real.tcBitPrecision = ()
      }

{-------------------------------------------------------------------------------
  Model state
-------------------------------------------------------------------------------}

type ModelState :: ((Type -> Type) -> Type -> Type -> Type -> Type) -> Type
data ModelState h = ModelState Model.Model Stats
  deriving Show

initModelState :: ModelState h
initModelState = ModelState Model.initModel initStats

{-------------------------------------------------------------------------------
  Type synonyms
-------------------------------------------------------------------------------}

type Act h a = Action (Lockstep (ModelState h)) (Either Model.Err a)
type Var h a = ModelVar (ModelState h) a
type Val h a = ModelValue (ModelState h) a
type Obs h a = Observable (ModelState h) a

type C_ a = (
    Model.C_ a
  , Model.SomeSerialisationConstraint a
  , SUT.SomeSerialisationConstraint a
  )

-- | Common constraints for keys, values and blobs
type C k v blob = (C_ k, C_ v, C_ blob)

{-------------------------------------------------------------------------------
  StateModel
-------------------------------------------------------------------------------}

instance ( Show (SUT.Class.TableConfig h)
         , Eq (SUT.Class.TableConfig h)
         , Arbitrary (SUT.Class.TableConfig h)
         , Typeable h
         ) => StateModel (Lockstep (ModelState h)) where
  -- TODO: add missing operations like listSnapshot and deleteSnapshot
  data instance Action (Lockstep (ModelState h)) a where
    -- Tables
    New :: C k v blob
        => SUT.Class.TableConfig h
        -> Act h (WrapTableHandle h IO k v blob)
    Close :: C k v blob
          => Var h (WrapTableHandle h IO k v blob)
          -> Act h ()
    -- Table queriehs
    Lookups :: C k v blob
            => [k] -> Var h (WrapTableHandle h IO k v blob)
            -> Act h [SUT.LookupResult k v (WrapBlobRef h IO blob)]
    RangeLookup :: C k v blob
                => SUT.Range k -> Var h (WrapTableHandle h IO k v blob)
                -> Act h [SUT.RangeLookupResult k v (WrapBlobRef h IO blob)]
    -- Updates
    Updates :: C k v blob
            => [(k, SUT.Update v blob)] -> Var h (WrapTableHandle h IO k v blob)
            -> Act h ()
    Inserts :: C k v blob
            => [(k, v, Maybe blob)] -> Var h (WrapTableHandle h IO k v blob)
            -> Act h ()
    Deletes :: C k v blob
            => [k] -> Var h (WrapTableHandle h IO k v blob)
            -> Act h ()
    -- Blobs
    RetrieveBlobs :: C_ blob
                  => Var h [WrapBlobRef h IO blob]
                  -> Act h [WrapBlob blob]
    -- Snapshots
    Snapshot :: C k v blob
             => SUT.SnapshotName -> Var h (WrapTableHandle h IO k v blob)
             -> Act h ()
    Open     :: C k v blob
             => SUT.SnapshotName
             -> Act h (WrapTableHandle h IO k v blob)
    DeleteSnapshot :: SUT.SnapshotName -> Act h ()
    ListSnapshots  :: Act h [SUT.SnapshotName]
    -- Multiple writable table handles
    Duplicate :: C k v blob
              => Var h (WrapTableHandle h IO k v blob)
              -> Act h (WrapTableHandle h IO k v blob)

  initialState    = Lockstep.Defaults.initialState initModelState
  nextState       = Lockstep.Defaults.nextState
  precondition    = Lockstep.Defaults.precondition
  arbitraryAction = Lockstep.Defaults.arbitraryAction
  shrinkAction    = Lockstep.Defaults.shrinkAction

-- TODO: show instance does not show key-value-blob types. Example:
--
-- Normal.StateMachine
--   propLockstepIO_ModelIOImpl: FAIL
--     *** Failed! Exception: 'open: inappropriate type (table type mismatch)' (after 25 tests and 2 shrinks):
--     do action $ New TableConfig
--        action $ Snapshot "snap" (GVar var1 (FromRight . id))
--        action $ Open "snap"
--        pure ()
deriving instance Show (SUT.Class.TableConfig h)
               => Show (LockstepAction (ModelState h) a)

instance ( Eq (SUT.Class.TableConfig h)
         , Typeable h
         ) => Eq (LockstepAction (ModelState h) a) where
  (==) :: LockstepAction (ModelState h) a -> LockstepAction (ModelState h) a -> Bool
  x == y = go x y
    where
      go :: LockstepAction (ModelState h) a -> LockstepAction (ModelState h) a -> Bool
      go (New conf1)                (New conf2) =
          conf1 == conf2
      go (Close var1)               (Close var2) =
          Just var1 == cast var2
      go (Lookups ks1 var1)         (Lookups ks2 var2) =
          ks1 == ks2 && var1 == var2
      go (RangeLookup range1 var1)  (RangeLookup range2 var2) =
          range1 == range2 && var1 == var2
      go (Updates ups1 var1)        (Updates ups2 var2) =
          Just ups1 == cast ups2 && Just var1 == cast var2
      go (Inserts inss1 var1)       (Inserts inss2 var2) =
          Just inss1 == cast inss2 && Just var1 == cast var2
      go (Deletes ks1 var1)         (Deletes ks2 var2) =
          Just ks1 == cast ks2 && Just var1 == cast var2
      go (RetrieveBlobs vars1) (RetrieveBlobs vars2) =
          Just vars1 == cast vars2
      go (Snapshot name1 var1)      (Snapshot name2 var2) =
          name1 == name2 && Just var1 == cast var2
      go (Open name1)               (Open name2) =
          name1 == name2
      go (DeleteSnapshot name1)     (DeleteSnapshot name2) =
          name1 == name2
      go ListSnapshots ListSnapshots =
          True
      go (Duplicate var1) (Duplicate var2) =
          Just var1 == cast var2
      go _  _ = False

      _coveredAllCases :: LockstepAction (ModelState h) a -> ()
      _coveredAllCases = \case
          New{} -> ()
          Close{} -> ()
          Lookups{} -> ()
          RangeLookup{} -> ()
          Updates{} -> ()
          Inserts{} -> ()
          Deletes{} -> ()
          RetrieveBlobs{} -> ()
          Snapshot{} -> ()
          DeleteSnapshot{} -> ()
          ListSnapshots{} -> ()
          Open{} -> ()
          Duplicate{} -> ()

{-------------------------------------------------------------------------------
  InLockstep
-------------------------------------------------------------------------------}

instance ( Eq (SUT.Class.TableConfig h)
         , Show (SUT.Class.TableConfig h)
         , Arbitrary (SUT.Class.TableConfig h)
         , Typeable h
         ) => InLockstep (ModelState h) where
  type instance ModelOp (ModelState h) = Op

  data instance ModelValue (ModelState h) a where
    MTableHandle :: Model.TableHandle k v blob
                 -> Val h (WrapTableHandle h IO k v blob)
    MBlobRef :: Model.C_ blob
             => Model.BlobRef blob -> Val h (WrapBlobRef h IO blob)

    MLookupResult :: Model.C k v blob
                  => Model.LookupResult k v (Val h (WrapBlobRef h IO blob))
                  -> Val h (SUT.LookupResult k v (WrapBlobRef h IO blob))
    MRangeLookupResult :: Model.C k v blob
                       => Model.RangeLookupResult k v (Val h (WrapBlobRef h IO blob))
                       -> Val h (SUT.RangeLookupResult k v (WrapBlobRef h IO blob))

    MBlob :: (Show blob, Typeable blob, Eq blob)
          => WrapBlob blob -> Val h (WrapBlob blob)
    MSnapshotName :: SUT.SnapshotName -> Val h SUT.SnapshotName
    MErr :: Model.Err -> Val h Model.Err

    MUnit   :: () -> Val h ()
    MPair   :: (Val h a, Val h b) -> Val h (a, b)
    MEither :: Either (Val h a) (Val h b) -> Val h (Either a b)
    MList   :: [Val h a] -> Val h [a]

  data instance Observable (ModelState h) a where
    OTableHandle :: Obs h (WrapTableHandle h IO k v blob)
    OBlobRef :: Obs h (WrapBlobRef h IO blob)

    -- TODO: can we use OId for lookup results and range lookup results instead,
    -- or are these separate constructors necessary?
    OLookupResult :: Model.C k v blob
                  => Model.LookupResult k v (Obs h (WrapBlobRef h IO blob))
                  -> Obs h (SUT.LookupResult k v (WrapBlobRef h IO blob))
    ORangeLookupResult :: Model.C k v blob
                       => Model.RangeLookupResult k v (Obs h (WrapBlobRef h IO blob))
                       -> Obs h (SUT.RangeLookupResult k v (WrapBlobRef h IO blob))

    OId :: (Show a, Typeable a, Eq a) => a -> Obs h a

    OPair   :: (Obs h a, Obs h b) -> Obs h (a, b)
    OEither :: Either (Obs h a) (Obs h b) -> Obs h (Either a b)
    OList   :: [Obs h a] -> Obs h [a]

  observeModel :: Val h a -> Obs h a
  observeModel = \case
      MTableHandle _       -> OTableHandle
      MBlobRef _           -> OBlobRef
      MLookupResult x      -> OLookupResult $ fmap observeModel x
      MRangeLookupResult x -> ORangeLookupResult $ fmap observeModel x
      MSnapshotName x      -> OId x
      MBlob x              -> OId x
      MErr x               -> OId x
      MUnit x              -> OId x
      MPair x              -> OPair $ bimap observeModel observeModel x
      MEither x            -> OEither $ bimap observeModel observeModel x
      MList x              -> OList $ map observeModel x

  modelNextState ::  forall a.
       LockstepAction (ModelState h) a
    -> ModelLookUp (ModelState h)
    -> ModelState h
    -> (ModelValue (ModelState h) a, ModelState h)
  modelNextState action lookUp (ModelState mock stats) =
      auxStats $ runModel lookUp action mock
    where
      auxStats :: (Val h a, Model.Model) -> (Val h a, ModelState h)
      auxStats (result, state') =
          (result, ModelState state' $ updateStats action result stats)

  usedVars :: LockstepAction (ModelState h) a -> [AnyGVar (ModelOp (ModelState h))]
  usedVars = \case
      New _                           -> []
      Close tableVar                  -> [SomeGVar tableVar]
      Lookups _ tableVar              -> [SomeGVar tableVar]
      RangeLookup _ tableVar          -> [SomeGVar tableVar]
      Updates _ tableVar              -> [SomeGVar tableVar]
      Inserts _ tableVar              -> [SomeGVar tableVar]
      Deletes _ tableVar              -> [SomeGVar tableVar]
      RetrieveBlobs blobsVar          -> [SomeGVar blobsVar]
      Snapshot _ tableVar             -> [SomeGVar tableVar]
      Open _                          -> []
      DeleteSnapshot _                -> []
      ListSnapshots                   -> []
      Duplicate tableVar              -> [SomeGVar tableVar]

  arbitraryWithVars ::
       ModelFindVariables (ModelState h)
    -> ModelState h
    -> Gen (Any (LockstepAction (ModelState h)))
  arbitraryWithVars findVars st = QC.oneof [
        arbitraryActionWithVars (Proxy @(Word64, Word64, Word64)) findVars st
      , arbitraryActionWithVars (Proxy @(BS.ByteString, BS.ByteString, BS.ByteString)) findVars st
      ]

  shrinkWithVars ::
       ModelFindVariables (ModelState h)
    -> ModelState h
    -> LockstepAction (ModelState h) a
    -> [Any (LockstepAction (ModelState h))]
  shrinkWithVars = shrinkActionWithVars

  tagStep ::
       (ModelState h, ModelState h)
    -> LockstepAction (ModelState h) a
    -> Val h a
    -> [String]
  tagStep states action = map show . tagStep' states action

deriving instance Show (SUT.Class.TableConfig h) => Show (Val h a)
deriving instance Eq (Obs h a)
deriving instance Show (Obs h a)

{-------------------------------------------------------------------------------
  Real monad
-------------------------------------------------------------------------------}

-- | Uses 'WrapSession' such that we can define class instances that mention
-- 'RealMonad' in the class head. This is necessary because class heads can not
-- mention type families directly.
--
-- Also carries an exception handle that is specific to the table implementation
-- identified by the table handle @h@.
type RealMonad h m = ReaderT (WrapSession h m, Handler m (Maybe Model.Err)) m

{-------------------------------------------------------------------------------
  RunLockstep
-------------------------------------------------------------------------------}

instance ( Eq (SUT.Class.TableConfig h)
         , SUT.Class.IsTableHandle h
         , Show (SUT.Class.TableConfig h)
         , Arbitrary (SUT.Class.TableConfig h)
         , Typeable h
         ) => RunLockstep (ModelState h) (RealMonad h IO) where
  observeReal ::
       Proxy (RealMonad h IO)
    -> LockstepAction (ModelState h) a
    -> Realized (RealMonad h IO) a
    -> Obs h a
  observeReal _proxy action result = case action of
      New{}            -> OEither $ bimap OId (const OTableHandle) result
      Close{}          -> OEither $ bimap OId OId result
      Lookups{}        -> OEither $
          bimap OId (OList . fmap (OLookupResult . fmap (const OBlobRef))) result
      RangeLookup{}    -> OEither $
          bimap OId (OList . fmap (ORangeLookupResult . fmap (const OBlobRef))) result
      Updates{}        -> OEither $ bimap OId OId result
      Inserts{}        -> OEither $ bimap OId OId result
      Deletes{}        -> OEither $ bimap OId OId result
      RetrieveBlobs{}  -> OEither $ bimap OId (OList . fmap OId) result
      Snapshot{}       -> OEither $ bimap OId OId result
      Open{}           -> OEither $ bimap OId (const OTableHandle) result
      DeleteSnapshot{} -> OEither $ bimap OId OId result
      ListSnapshots{}  -> OEither $ bimap OId (OList . fmap OId) result
      Duplicate{}      -> OEither $ bimap OId (const OTableHandle) result

  showRealResponse ::
       Proxy (RealMonad h IO)
    -> LockstepAction (ModelState h) a
    -> Maybe (Dict (Show (Realized (RealMonad h IO) a)))
  showRealResponse _ = \case
      New{}            -> Nothing
      Close{}          -> Just Dict
      Lookups{}        -> Nothing
      RangeLookup{}    -> Nothing
      Updates{}        -> Just Dict
      Inserts{}        -> Just Dict
      Deletes{}        -> Just Dict
      RetrieveBlobs{}  -> Just Dict
      Snapshot{}       -> Just Dict
      Open{}           -> Nothing
      DeleteSnapshot{} -> Just Dict
      ListSnapshots    -> Just Dict
      Duplicate{}      -> Nothing

instance ( Eq (SUT.Class.TableConfig h)
         , SUT.Class.IsTableHandle h
         , Show (SUT.Class.TableConfig h)
         , Arbitrary (SUT.Class.TableConfig h)
         , Typeable h
         ) => RunLockstep (ModelState h) (RealMonad h (IOSim s)) where
  observeReal ::
       Proxy (RealMonad h (IOSim s))
    -> LockstepAction (ModelState h) a
    -> Realized (RealMonad h (IOSim s)) a
    -> Obs h a
  observeReal _proxy action result = case action of
      New{}            -> OEither $ bimap OId (const OTableHandle) result
      Close{}          -> OEither $ bimap OId OId result
      Lookups{}        -> OEither $
          bimap OId (OList . fmap (OLookupResult . fmap (const OBlobRef))) result
      RangeLookup{}    -> OEither $
          bimap OId (OList . fmap (ORangeLookupResult . fmap (const OBlobRef))) result
      Updates{}        -> OEither $ bimap OId OId result
      Inserts{}        -> OEither $ bimap OId OId result
      Deletes{}        -> OEither $ bimap OId OId result
      RetrieveBlobs{}  -> OEither $ bimap OId (OList . fmap OId) result
      Snapshot{}       -> OEither $ bimap OId OId result
      Open{}           -> OEither $ bimap OId (const OTableHandle) result
      DeleteSnapshot{} -> OEither $ bimap OId OId result
      ListSnapshots{}  -> OEither $ bimap OId OId result
      Duplicate{}      -> OEither $ bimap OId (const OTableHandle) result

  showRealResponse ::
       Proxy (RealMonad h (IOSim s))
    -> LockstepAction (ModelState h) a
    -> Maybe (Dict (Show (Realized (RealMonad h (IOSim s)) a)))
  showRealResponse _ = \case
      New{}            -> Nothing
      Close{}          -> Just Dict
      Lookups{}        -> Nothing
      RangeLookup{}    -> Nothing
      Updates{}        -> Just Dict
      Inserts{}        -> Just Dict
      Deletes{}        -> Just Dict
      RetrieveBlobs{}  -> Just Dict
      Snapshot{}       -> Just Dict
      Open{}           -> Nothing
      DeleteSnapshot{} -> Just Dict
      ListSnapshots    -> Just Dict
      Duplicate{}      -> Nothing

{-------------------------------------------------------------------------------
  RunModel
-------------------------------------------------------------------------------}

instance ( Eq (SUT.Class.TableConfig h)
         , SUT.Class.IsTableHandle h
         , Show (SUT.Class.TableConfig h)
         , Arbitrary (SUT.Class.TableConfig h)
         , Typeable h
         ) => RunModel (Lockstep (ModelState h)) (RealMonad h IO) where
  perform _     = runIO
  postcondition = Lockstep.Defaults.postcondition
  monitoring    = Lockstep.Defaults.monitoring (Proxy @(RealMonad h IO))

instance ( Eq (SUT.Class.TableConfig h)
         , SUT.Class.IsTableHandle h
         , Show (SUT.Class.TableConfig h)
         , Arbitrary (SUT.Class.TableConfig h)
         , Typeable h
         ) => RunModel (Lockstep (ModelState h)) (RealMonad h (IOSim s)) where
  perform _     = runIOSim
  postcondition = Lockstep.Defaults.postcondition
  monitoring    = Lockstep.Defaults.monitoring (Proxy @(RealMonad h (IOSim s)))

{-------------------------------------------------------------------------------
  Interpreter for the model
-------------------------------------------------------------------------------}

runModel ::
     ModelLookUp (ModelState h)
  -> LockstepAction (ModelState h) a
  -> Model.Model -> (Val h a, Model.Model)
runModel lookUp = \case
    New _cfg -> wrap MTableHandle .
      Model.runModelM (Model.new Model.TableConfig)
    Close tableVar -> wrap MUnit .
      Model.runModelM (Model.close (getTableHandle $ lookUp tableVar))
    Lookups ks tableVar -> wrap (MList . fmap (MLookupResult . fmap MBlobRef)) .
      Model.runModelM (Model.lookups ks (getTableHandle $ lookUp tableVar))
    RangeLookup range tableVar -> wrap (MList . fmap (MRangeLookupResult . fmap MBlobRef)) .
      Model.runModelM (Model.rangeLookup range (getTableHandle $ lookUp tableVar))
    Updates kups tableVar -> wrap MUnit .
      Model.runModelM (Model.updates kups (getTableHandle $ lookUp tableVar))
    Inserts kins tableVar -> wrap MUnit .
      Model.runModelM (Model.inserts kins (getTableHandle $ lookUp tableVar))
    Deletes kdels tableVar -> wrap MUnit .
      Model.runModelM (Model.deletes kdels (getTableHandle $ lookUp tableVar))
    RetrieveBlobs blobsVar -> wrap (MList . fmap (MBlob . WrapBlob)) .
      Model.runModelM (Model.retrieveBlobs (getBlobRefs . lookUp $ blobsVar))
    Snapshot name tableVar -> wrap MUnit .
      Model.runModelM (Model.snapshot name (getTableHandle $ lookUp tableVar))
    Open name -> wrap MTableHandle .
      Model.runModelM (Model.open name)
    DeleteSnapshot name -> wrap MUnit .
      Model.runModelM (Model.deleteSnapshot name)
    ListSnapshots -> wrap (MList . fmap MSnapshotName) .
      Model.runModelM Model.listSnapshots
    Duplicate tableVar -> wrap MTableHandle .
      Model.runModelM (Model.duplicate (getTableHandle $ lookUp tableVar))
  where
    getTableHandle ::
         ModelValue (ModelState h) (WrapTableHandle h IO k v blob)
      -> Model.TableHandle k v blob
    getTableHandle (MTableHandle th) = th

    getBlobRefs :: ModelValue (ModelState h) [WrapBlobRef h IO blob] -> [Model.BlobRef blob]
    getBlobRefs (MList brs) = fmap (\(MBlobRef br) -> br) brs

wrap ::
     (a -> Val h b)
  -> (Either Model.Err a, Model.Model)
  -> (Val h (Either Model.Err b), Model.Model)
wrap f = first (MEither . bimap MErr f)

{-------------------------------------------------------------------------------
  Interpreters for @'IOLike' m@
-------------------------------------------------------------------------------}

runIO ::
     forall a h. SUT.Class.IsTableHandle h
  => LockstepAction (ModelState h) a
  -> LookUp (RealMonad h IO)
  -> RealMonad h IO (Realized (RealMonad h IO) a)
runIO action lookUp = ReaderT $ \(session, handler) ->
    aux (unwrapSession session) handler action
  where
    aux ::
         SUT.Class.Session h IO
      -> Handler IO (Maybe Model.Err)
      -> LockstepAction (ModelState h) a
      -> IO (Realized IO a)
    aux session handler = \case
        New cfg -> catchErr handler $
          WrapTableHandle <$> SUT.Class.new session cfg
        Close tableVar -> catchErr handler $
          SUT.Class.close (unwrapTableHandle $ lookUp' tableVar)
        Lookups ks tableVar -> catchErr handler $
          fmap (fmap WrapBlobRef) <$> SUT.Class.lookups (unwrapTableHandle $ lookUp' tableVar) ks
        RangeLookup range tableVar -> catchErr handler $
          fmap (fmap WrapBlobRef) <$> SUT.Class.rangeLookup (unwrapTableHandle $ lookUp' tableVar) range
        Updates kups tableVar -> catchErr handler $
          SUT.Class.updates (unwrapTableHandle $ lookUp' tableVar) kups
        Inserts kins tableVar -> catchErr handler $
          SUT.Class.inserts (unwrapTableHandle $ lookUp' tableVar) kins
        Deletes kdels tableVar -> catchErr handler $
          SUT.Class.deletes (unwrapTableHandle $ lookUp' tableVar) kdels
        RetrieveBlobs blobRefsVar -> catchErr handler $
          fmap WrapBlob <$> SUT.Class.retrieveBlobs (Proxy @h) (unwrapBlobRef <$> lookUp' blobRefsVar)
        Snapshot name tableVar -> catchErr handler $
          SUT.Class.snapshot name (unwrapTableHandle $ lookUp' tableVar)
        Open name -> catchErr handler $
          WrapTableHandle <$> SUT.Class.open session name
        DeleteSnapshot name -> catchErr handler $
          SUT.Class.deleteSnapshot session name
        ListSnapshots -> catchErr handler $
          SUT.Class.listSnapshots session
        Duplicate tableVar -> catchErr handler $
          WrapTableHandle <$> SUT.Class.duplicate (unwrapTableHandle $ lookUp' tableVar)

    lookUp' :: Var j x -> Realized IO x
    lookUp' = lookUpGVar (Proxy @(RealMonad h IO)) lookUp

runIOSim ::
     forall s a h. SUT.Class.IsTableHandle h
  => LockstepAction (ModelState h) a
  -> LookUp (RealMonad h (IOSim s))
  -> RealMonad h (IOSim s) (Realized (RealMonad h (IOSim s)) a)
runIOSim action lookUp = ReaderT $ \(session, handler) ->
    aux (unwrapSession session) handler action
  where
    aux ::
         SUT.Class.Session h (IOSim s)
      -> Handler (IOSim s) (Maybe Model.Err)
      -> LockstepAction (ModelState h) a
      -> IOSim s (Realized (IOSim s) a)
    aux session handler = \case
        New cfg -> catchErr handler $
          WrapTableHandle <$> SUT.Class.new session cfg
        Close tableVar -> catchErr handler $
          SUT.Class.close (unwrapTableHandle $ lookUp' tableVar)
        Lookups ks tableVar -> catchErr handler $
          fmap (fmap WrapBlobRef) <$> SUT.Class.lookups (unwrapTableHandle $ lookUp' tableVar) ks
        RangeLookup range tableVar -> catchErr handler $
          fmap (fmap WrapBlobRef) <$> SUT.Class.rangeLookup (unwrapTableHandle $ lookUp' tableVar) range
        Updates kups tableVar -> catchErr handler $
          SUT.Class.updates (unwrapTableHandle $ lookUp' tableVar) kups
        Inserts kins tableVar -> catchErr handler $
          SUT.Class.inserts (unwrapTableHandle $ lookUp' tableVar) kins
        Deletes kdels tableVar -> catchErr handler $
          SUT.Class.deletes (unwrapTableHandle $ lookUp' tableVar) kdels
        RetrieveBlobs blobRefsVar -> catchErr handler $
          fmap WrapBlob <$> SUT.Class.retrieveBlobs (Proxy @h) (unwrapBlobRef <$> lookUp' blobRefsVar)
        Snapshot name tableVar -> catchErr handler $
          SUT.Class.snapshot name (unwrapTableHandle $ lookUp' tableVar)
        Open name -> catchErr handler $
          WrapTableHandle <$> SUT.Class.open session name
        DeleteSnapshot name -> catchErr handler $
          SUT.Class.deleteSnapshot session name
        ListSnapshots -> catchErr handler $
          SUT.Class.listSnapshots session
        Duplicate tableVar -> catchErr handler $
          WrapTableHandle <$> SUT.Class.duplicate (unwrapTableHandle $ lookUp' tableVar)
    lookUp' :: Var h x -> Realized (IOSim s) x
    lookUp' = lookUpGVar (Proxy @(RealMonad h (IOSim s))) lookUp

catchErr ::
     forall m a. MonadCatch m
  => Handler m (Maybe Model.Err) -> m a -> m (Either Model.Err a)
catchErr (Handler f) action = catch (Right <$> action) f'
  where
    f' e = maybe (throwIO e) (pure . Left) =<< f e

{-------------------------------------------------------------------------------
  Generator and shrinking
-------------------------------------------------------------------------------}

arbitraryActionWithVars ::
     forall h k v blob. (
       C k v blob, Arbitrary k, Arbitrary v, Arbitrary blob
     , Eq (SUT.Class.TableConfig h)
     , Arbitrary (SUT.Class.TableConfig h)
     , Typeable h
     )
  => Proxy (k, v, blob)
  -> ModelFindVariables (ModelState h)
  -> ModelState h
  -> Gen (Any (LockstepAction (ModelState h)))
arbitraryActionWithVars _ findVars _st = QC.oneof $ concat [
      withoutVars
    , case findVars (Proxy @(Either Model.Err (WrapTableHandle h IO k v blob))) of
        []   -> []
        vars -> withVars (QC.elements vars)
    , case findBlobRefsVars of
        []    -> []
        vars' -> withVars' (QC.elements vars')
    ]
  where
    _coveredAllCases :: LockstepAction (ModelState h) a -> ()
    _coveredAllCases = \case
        New{} -> ()
        Close{} -> ()
        Lookups{} -> ()
        RangeLookup{} -> ()
        Updates{} -> ()
        Inserts{} -> ()
        Deletes{} -> ()
        RetrieveBlobs{} -> ()
        Snapshot{} -> ()
        DeleteSnapshot{} -> ()
        ListSnapshots{} -> ()
        Open{} -> ()
        Duplicate{} -> ()

    findBlobRefsVars :: [Var h (Either Model.Err [WrapBlobRef h IO blob])]
    findBlobRefsVars = fmap fromLookupResults vars1 ++ fmap fromRangeLookupResults vars2
      where
        vars1 = findVars (Proxy @(Either Model.Err [SUT.LookupResult k v (WrapBlobRef h IO blob)]))
        vars2 = findVars (Proxy @(Either Model.Err [SUT.RangeLookupResult k v (WrapBlobRef h IO blob)]))

        fromLookupResults ::
             Var h (Either Model.Err [SUT.LookupResult k v (WrapBlobRef h IO blob)])
          -> Var h (Either Model.Err [WrapBlobRef h IO blob])
        fromLookupResults = mapGVar (\op -> OpRight `OpComp` OpLookupResults `OpComp` OpFromRight `OpComp` op)

        fromRangeLookupResults ::
             Var h (Either Model.Err [SUT.RangeLookupResult k v (WrapBlobRef h IO blob)])
          -> Var h (Either Model.Err [WrapBlobRef h IO blob])
        fromRangeLookupResults = mapGVar (\op -> OpRight `OpComp` OpRangeLookupResults `OpComp` OpFromRight `OpComp` op)

    withoutVars :: [Gen (Any (LockstepAction (ModelState h)))]
    withoutVars = [
          Some . New @k @v @blob <$> QC.arbitrary
        , fmap Some $ Open @k @v @blob <$> genSnapshotName
        , fmap Some $ DeleteSnapshot <$> genSnapshotName
        , pure $ Some ListSnapshots
        ]

    withVars ::
         Gen (Var h (Either Model.Err (WrapTableHandle h IO k v blob)))
      -> [Gen (Any (LockstepAction (ModelState h)))]
    withVars genVar = [
          fmap Some $ Close <$> (fromRight <$> genVar)
        , fmap Some $ Lookups <$> genLookupKeys <*> (fromRight <$> genVar)
        , fmap Some $ RangeLookup <$> genRange <*> (fromRight <$> genVar)
        , fmap Some $ Updates <$> genUpdates <*> (fromRight <$> genVar)
        , fmap Some $ Inserts <$> genInserts <*> (fromRight <$> genVar)
        , fmap Some $ Deletes <$> genDeletes <*> (fromRight <$> genVar)
        , fmap Some $ Snapshot <$> genSnapshotName <*> (fromRight <$> genVar)
        , fmap Some $ Duplicate <$> (fromRight <$> genVar)
        ]

    withVars' ::
         Gen (Var h (Either Model.Err [WrapBlobRef h IO blob]))
      -> [Gen (Any (LockstepAction (ModelState h)))]
    withVars' genBlobRefsVar = [
          fmap Some $ RetrieveBlobs <$> (fromRight <$> genBlobRefsVar)
        ]

    fromRight ::
         Var h (Either Model.Err a)
      -> Var h a
    fromRight = mapGVar (\op -> OpFromRight `OpComp` op)

    -- TODO: improve
    genLookupKeys :: Gen [k]
    genLookupKeys = QC.arbitrary

    -- TODO: improve
    genRange :: Gen (SUT.Range k)
    genRange = QC.oneof [
          SUT.FromToExcluding <$> QC.arbitrary <*> QC.arbitrary
        , SUT.FromToIncluding <$> QC.arbitrary <*> QC.arbitrary
        ]
      where
        _coveredAllCases :: SUT.Range k -> ()
        _coveredAllCases = \case
            SUT.FromToExcluding{} -> ()
            SUT.FromToIncluding{} -> ()

    -- TODO: improve
    genUpdates :: Gen [(k, SUT.Update v blob)]
    genUpdates = QC.listOf $ (,) <$> QC.arbitrary <*> QC.oneof [
          SUT.Insert <$> QC.arbitrary <*> QC.arbitrary
        , pure SUT.Delete
        ]
      where
        _coveredAllCases :: SUT.Update v blob -> ()
        _coveredAllCases = \case
            SUT.Insert{} -> ()
            SUT.Delete{} -> ()

    -- TODO: improve
    genInserts :: Gen [(k, v, Maybe blob)]
    genInserts = QC.arbitrary

    -- TODO: improve
    genDeletes :: Gen [k]
    genDeletes = QC.arbitrary

    -- TODO: improve, actual snapshot names
    genSnapshotName :: Gen SUT.SnapshotName
    genSnapshotName = QC.elements [
        fromJust $ SUT.mkSnapshotName "snap1"
      , fromJust $ SUT.mkSnapshotName "snap2"
      , fromJust $ SUT.mkSnapshotName "snap3"
      ]

shrinkActionWithVars ::
       ModelFindVariables (ModelState h)
    -> ModelState h
    -> LockstepAction (ModelState h) a
    -> [Any (LockstepAction (ModelState h))]
shrinkActionWithVars _ _ _ = []

{-------------------------------------------------------------------------------
  Interpret 'Op' against 'ModelValue'
-------------------------------------------------------------------------------}

instance InterpretOp Op (ModelValue (ModelState h)) where
  intOp = \case
    OpId                 -> Just
    OpFst                -> \case MPair   x -> Just (fst x)
    OpSnd                -> \case MPair   x -> Just (snd x)
    OpLeft               -> Just . MEither . Left
    OpRight              -> Just . MEither . Right
    OpFromLeft           -> \case MEither x -> either Just (const Nothing) x
    OpFromRight          -> \case MEither x -> either (const Nothing) Just x
    OpComp g f           -> intOp g <=< intOp f
    OpLookupResults      -> Just . MList
                          . mapMaybe (\case MLookupResult x -> getBlobRef x)
                          . \case MList x -> x
    OpRangeLookupResults -> Just . MList
                          . mapMaybe (\case MRangeLookupResult x -> getBlobRef x)
                          . \case MList x -> x

{-------------------------------------------------------------------------------
  Statistics, labelling/tagging
-------------------------------------------------------------------------------}

data ConstructorName =
    CNew
  | CClose
  | CLookups
  | CRangeLookup
  | CUpdates
  | CInserts
  | CDeletes
  | CRetrieveBlobs
  | CSnapshot
  | COpen
  | CDeleteSnapshot
  | CListSnapshots
  | CDuplicate
  deriving (Show, Eq, Ord, Enum, Bounded)

toConstructorName :: LockstepAction (ModelState h) a -> ConstructorName
toConstructorName = \case
    New{} -> CNew
    Close{} -> CClose
    Lookups{} -> CLookups
    RangeLookup{} -> CRangeLookup
    Updates{} -> CUpdates
    Inserts{} -> CInserts
    Deletes{} -> CDeletes
    RetrieveBlobs{} -> CRetrieveBlobs
    Snapshot{} -> CSnapshot
    Open{} -> COpen
    DeleteSnapshot{} -> CDeleteSnapshot
    ListSnapshots{} -> CListSnapshots
    Duplicate{} -> CDuplicate

newtype Stats = Stats {
    seenActions :: Set ConstructorName
  }
  deriving Show

initStats :: Stats
initStats = Stats {
      seenActions = Set.empty
    }

updateStats :: LockstepAction (ModelState h) a -> Val h a -> Stats -> Stats
updateStats action _result Stats{seenActions} = Stats {
      seenActions = Set.insert (toConstructorName action) seenActions
    }

data Tag =
    -- | Trivial tag
    Top
    -- | All actions were seen at least once
  | All
  deriving Show

tagStep' ::
     (ModelState h, ModelState h)
  -> LockstepAction (ModelState h) a
  -> Val h a
  -> [Tag]
tagStep' (_before, ModelState _ statsAfter) _action _result = Top :
    [All | seenActions statsAfter == Set.fromList [minBound .. maxBound]]
