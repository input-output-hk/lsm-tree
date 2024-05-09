{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Test.Database.LSMTree.Internal.Merge.Readers (tests) where

import           Control.Monad (zipWithM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bifunctor (bimap, first)
import           Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import           Database.LSMTree.Extras.Generators (KeyForIndexCompact,
                     TypedWriteBuffer (..))
import           Database.LSMTree.Internal.Entry
import           Database.LSMTree.Internal.BlobRef
import           Database.LSMTree.Internal.Merge (HasMore (Drained, HasMore))
import qualified Database.LSMTree.Internal.Merge as Merge
import qualified Database.LSMTree.Internal.Run as Run
import qualified Database.LSMTree.Internal.RunReader as Reader
import           Database.LSMTree.Internal.RunFsPaths (RunFsPaths (..))
import           Database.LSMTree.Internal.Serialise
import qualified Database.LSMTree.Internal.WriteBuffer as WB
import qualified System.FS.API as FS
import qualified Test.QuickCheck as QC
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans ()
import           Data.Proxy (Proxy (..))
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Control.Monad.Trans.State (StateT (..), evalStateT, get, put)
import qualified System.FS.Sim.STM as FsSim
import qualified System.FS.Sim.MockFS as FsSim

import Test.QuickCheck.StateModel
import Test.QuickCheck.StateModel.Lockstep
import Test.QuickCheck.StateModel.Lockstep.Run      qualified as Lockstep
import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep

tests :: TestTree
tests = testGroup "Database.LSMTree.Internal.Merge.Readers"
    [ testProperty "prop_lockstep" $
        Lockstep.runActionsBracket (Proxy @ReadersState)
          ((,) <$> FsSim.simHasFS' FsSim.empty <*> pure (RealState 0 Nothing))
          mempty
          (\act (fs, st) -> runRealMonad fs st act)
    ]

--------------------------------------------------------------------------------

type RealEntry = MockEntry
type RawEntry = Reader.RawEntry Handle

type Handle = FS.Handle FsSim.HandleMock

--------------------------------------------------------------------------------
-- Mock

newtype MockReaders = MockReaders [((SerialisedKey, RunNumber), MockEntry)]
  deriving Show

newtype RunNumber = RunNumber Int
  deriving (Eq, Ord, Show)

type MockEntry = Entry SerialisedValue SerialisedBlob

isEmpty :: MockReaders -> Bool
isEmpty (MockReaders xs) = null xs

newMock :: [WB.WriteBuffer] -> MockReaders
newMock =
      MockReaders . Map.assocs . Map.unions
    . zipWith (\i -> Map.mapKeysMonotonic (\k -> (k, RunNumber i)) . WB.unWB) [0..]

peekKeyMock :: MockReaders -> Either () SerialisedKey
peekKeyMock (MockReaders xs) = case xs of
    [] -> Left ()
    (((k, _), _) : _) -> Right k

popMock :: MockReaders -> (Either () (SerialisedKey, MockEntry, HasMore), MockReaders)
popMock m@(MockReaders xs) = case xs of
    [] ->
      (Left (), m)
    (((k, _), e) : rest) ->
      (Right (k, e, toHasMore rest), MockReaders rest)

dropWhileKeyMock :: SerialisedKey -> MockReaders -> (Either () (Int, HasMore), MockReaders)
dropWhileKeyMock k m@(MockReaders xs)
  | null xs = (Left (), m)
  | otherwise =
      let (dropped, xs') = span ((== k) . fst . fst) xs
      in (Right (length dropped, toHasMore xs'), MockReaders xs')

toHasMore :: [a] -> HasMore
toHasMore xs = if null xs then Drained else HasMore


data ReadersState = ReadersState MockReaders
  deriving Show

initReadersState :: ReadersState
initReadersState = ReadersState (newMock [])

--------------------------------------------------------------------------------

type ReadersAct a = Action (Lockstep ReadersState) (Either () a)

deriving instance Show (Action (Lockstep ReadersState) a)
deriving instance Eq   (Action (Lockstep ReadersState) a)

instance StateModel (Lockstep ReadersState) where
  data Action (Lockstep ReadersState) a where
    New          :: [TypedWriteBuffer KeyForIndexCompact SerialisedValue SerialisedBlob]
                 -> ReadersAct ()
    PeekKey      :: ReadersAct SerialisedKey
    Pop          :: ReadersAct (SerialisedKey, RealEntry, HasMore)
    DropWhileKey :: SerialisedKey
                 -> ReadersAct (Int, HasMore)

  initialState    = Lockstep.initialState initReadersState
  nextState       = Lockstep.nextState
  precondition    = Lockstep.precondition
  arbitraryAction = Lockstep.arbitraryAction
  shrinkAction    = Lockstep.shrinkAction

type ReadersVal a = ModelValue ReadersState a
type ReadersObs a = Observable ReadersState a

deriving instance Show (ReadersVal a)
deriving instance Show (ReadersObs a)
deriving instance Eq   (ReadersObs a)

instance InLockstep ReadersState where
  data ModelValue ReadersState a where
    MEntry  :: MockEntry -> ReadersVal RealEntry

    -- Rest is regular:
    MKey    :: SerialisedKey -> ReadersVal SerialisedKey
    MHasMore:: HasMore -> ReadersVal HasMore
    MInt    :: Int -> ReadersVal Int
    MUnit   :: () -> ReadersVal ()
    MEither :: Either (ReadersVal a) (ReadersVal b) -> ReadersVal (Either a b)
    MTuple2 :: (ReadersVal a, ReadersVal b)         -> ReadersVal (a, b)
    MTuple3 :: (ReadersVal a, ReadersVal b, ReadersVal c) -> ReadersVal (a, b, c)

  data Observable ReadersState a where
    OId     :: (Eq a, Show a) => a -> ReadersObs a
    OEither :: Either (ReadersObs a) (ReadersObs b) -> ReadersObs (Either a b)
    OTuple2 :: (ReadersObs a, ReadersObs b) -> ReadersObs (a, b)
    OTuple3 :: (ReadersObs a, ReadersObs b, ReadersObs c) -> ReadersObs (a, b, c)

  observeModel :: ReadersVal a -> ReadersObs a
  observeModel = \case
    -- Rest is regular:
    MEntry e -> OId e
    MKey k -> OId k
    MHasMore h -> OId h
    MInt n -> OId n
    MUnit () -> OId ()
    MEither x -> OEither $ bimap observeModel observeModel x
    MTuple2 x -> OTuple2 $ bimap observeModel observeModel x
    MTuple3 x -> OTuple3 $ trimap observeModel observeModel observeModel x

  modelNextState :: forall a.
       LockstepAction ReadersState a
    -> ModelLookUp ReadersState
    -> ReadersState
    -> (ReadersVal a, ReadersState)
  modelNextState action lookUp (ReadersState mock) =
      ReadersState <$> runMock lookUp action mock

  usedVars :: LockstepAction ReadersState a -> [AnyGVar (ModelOp ReadersState)]
  usedVars = const []

  arbitraryWithVars ::
       ModelFindVariables ReadersState
    -> ReadersState
    -> Gen (Any (LockstepAction ReadersState))
  arbitraryWithVars _ (ReadersState mock)
    | isEmpty mock =
        Some . New <$> arbitrary
    | otherwise =
        QC.frequency $
          [ (2, pure (Some PeekKey))
          , (4, pure (Some Pop))
          , (1, Some . DropWhileKey <$> arbitrary)
          ] <>
          [ (1, pure (Some (DropWhileKey k)))
          | Right k <- [peekKeyMock mock]
          ]

  shrinkWithVars ::
       ModelFindVariables ReadersState
    -> ReadersState
    -> LockstepAction ReadersState a
    -> [Any (LockstepAction ReadersState)]
  shrinkWithVars _ _ = \case
      New wbs -> Some . New <$> shrink wbs
      _       -> []

  tagStep _ _ _ = []

runMock ::
     lookUp
  -> Action (Lockstep ReadersState) a
  -> MockReaders
  -> (ReadersVal a, MockReaders)
runMock _ = \case
    New wbs        -> const $ wrap MUnit (Right (), newMock (map unTypedWriteBuffer wbs))
    PeekKey        -> \m -> wrap MKey (peekKeyMock m, m)
    Pop            -> wrap wrapPop . popMock
    DropWhileKey k -> wrap wrapDrop . dropWhileKeyMock k
  where
    wrap :: (a -> ReadersVal b) -> (Either () a, MockReaders) -> (ReadersVal (Either () b), MockReaders)
    wrap f = first (MEither . bimap MUnit f)

    wrapPop = MTuple3 . trimap MKey MEntry MHasMore

    wrapDrop = MTuple2 . bimap MInt MHasMore

trimap :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
trimap f g h (a, b, c) = (f a, g b, h c)

data Op a b where
  OpId   :: Op a a
  OpUnit :: Op a ()

instance Operation Op where
  opIdentity = OpId

instance InterpretOp Op (ModelValue ReadersState) where
  intOp OpId   x = Just x
  intOp OpUnit _ = Just (MUnit ())

type RealMonad = ReaderT (FS.HasFS IO FsSim.HandleMock) (StateT RealState IO)

runRealMonad :: FS.HasFS IO FsSim.HandleMock -> RealState -> RealMonad a -> IO a
runRealMonad fs st = (`evalStateT` st) . (`runReaderT` fs)

data RealState = RealState !Int !(Maybe (Merge.Readers Handle))

instance RunModel (Lockstep ReadersState) RealMonad where
  perform       = \_st -> runIO
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @RealMonad)

instance RunLockstep ReadersState RealMonad where
  observeReal _proxy = \case
      New {}          -> OEither . bimap OId OId
      PeekKey {}      -> OEither . bimap OId OId
      Pop {}          -> OEither . bimap OId (OTuple3 . trimap OId OId OId)
      DropWhileKey {} -> OEither . bimap OId (OTuple2 . bimap OId OId)

runIO :: LockstepAction ReadersState a -> LookUp RealMonad -> RealMonad (Realized RealMonad a)
runIO act _ = case act of
    New wbs -> ReaderT $ \fs -> do
      RealState numRuns mR <- get
      traverse_ (liftIO . Merge.readersClose fs) mR
      -- TODO: we could also delete the files
      runs <-
        zipWithM
          (\p -> liftIO . Run.fromWriteBuffer fs p)
          (RunFsPaths <$> [numRuns ..])
          (map unTypedWriteBuffer wbs)
      newReaders <- liftIO $ Merge.readersNew fs runs
      put (RealState (numRuns + length wbs) newReaders)
      return (Right ())
    PeekKey -> expectReaders $ \_ r -> do
      (,) HasMore <$> Merge.readersPeekKey r
    Pop -> expectReaders $ \fs r -> do
      (key, e, hasMore) <- Merge.readersPop fs r
      fullEntry <- toMockEntry fs e
      return (hasMore, (key, fullEntry, hasMore))
    DropWhileKey k -> expectReaders $ \fs r -> do
      (n, hasMore) <- Merge.readersDropWhileKey fs r k
      return (hasMore, (n, hasMore))
  where
    expectReaders ::
         (FS.HasFS IO FsSim.HandleMock -> Merge.Readers Handle -> IO (HasMore, a))
      -> RealMonad (Either () a)
    expectReaders f =
        ReaderT $ \fs -> do
          get >>= \case
            RealState _ Nothing -> return (Left ())
            RealState n (Just r) -> do
              (hasMore, x) <- liftIO $ f fs r
              case hasMore of
                HasMore ->
                  return (Right x)
                Drained -> do
                  put (RealState n Nothing)
                  return (Right x)

    toMockEntry :: FS.HasFS IO FsSim.HandleMock -> RawEntry -> IO MockEntry
    toMockEntry fs =
        traverse loadBlob . Reader.rawToFullEntry
      where
        loadBlob :: BlobRef (Run.Run Handle) -> IO SerialisedBlob
        loadBlob (BlobRef run sp) = Run.readBlob fs run sp
