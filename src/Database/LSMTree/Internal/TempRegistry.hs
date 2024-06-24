module Database.LSMTree.Internal.TempRegistry (
    TempRegistry
  , allocateTemp
  , allocateMaybeTemp
  , allocateEitherTemp
  , freeTemp
  , modifyWithTempRegistry
  , modifyWithTempRegistry_
  ) where

import           Control.Concurrent.Class.MonadMVar.Strict
import           Control.Monad.Class.MonadThrow
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Void

-- | A temporary registry for resources that are bound to end up in some final
-- state, after which they /should/ be guaranteed to be released correctly.
--
-- It is the responsibility of the user to guarantee that this final state is
-- released correctly in the presence of async exceptions.
--
-- NOTE: this is based on [the @ResourceRegistry@ module from @ouroboros-consensus@](https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Util/ResourceRegistry.hs).
--
-- There are some differences between @WithTempRegistry@ from
-- @ouroboros-consensus@ and our 'TempRegistry'. For one, 'TempRegistry' allows
-- for the temporary /freeing/ of resources, which @WithTempRegistry@ does not.
-- However, @WithTempRegistry@ can check whether newly allocated resources
-- actually end up in the final state.
--
-- TODO: make 'TempRegistry' more sophisticated. Ideas:
--
-- * Use a similar approach (like in 'WithTempRegistry@) for checking that
--   temporarily allocated resources end up in the final state, and that
--   temporarily freed resources are removed from the final state.
--
-- * Statically disallow using a resource after @freeTemp@, for example through
--   data abstraction.
newtype TempRegistry m = TempRegistry {
    tempRegistryState :: StrictMVar m (TempRegistryState m)
  }

data TempRegistryState m = TempRegistryState {
    tempAllocated :: !(Map ResourceId (Resource m))
  , tempFreed     :: !(Map ResourceId (Resource m))
  , nextId        :: !ResourceId
  }

newtype ResourceId = ResourceId Int
  deriving (Eq, Ord, Num)

newtype Resource m = Resource {
    resourceRelease :: (m ())
  }

{-# SPECIALISE newTempRegistry :: IO (TempRegistry IO) #-}
newTempRegistry :: MonadMVar m => m (TempRegistry m)
newTempRegistry = TempRegistry <$> newMVar (TempRegistryState Map.empty Map.empty (ResourceId 0))

{-# SPECIALISE unsafeReleaseTempRegistry :: TempRegistry IO -> ExitCase a -> IO () #-}
unsafeReleaseTempRegistry :: MonadMVar m => TempRegistry m -> ExitCase a -> m ()
unsafeReleaseTempRegistry reg ec = case ec of
    ExitCaseSuccess{} -> mapM_ resourceRelease . tempFreed     =<< takeMVar (tempRegistryState reg)
    _                 -> mapM_ resourceRelease . tempAllocated =<< takeMVar (tempRegistryState reg)


{-# SPECIALISE allocateTemp :: TempRegistry IO -> IO a -> (a -> IO ()) -> IO a #-}
-- | Temporarily allocate a resource.
--
-- This runs the @acquire@ function with async exceptions masked to ensure that
-- acquired resources are always put into the registry. However, note that in
-- general the following two expressions are not equivalent:
--
-- @
--   allocateTemp reg acquire
--   acquire >>= \x -> allocateTemp reg (pure x)
-- @
--
-- Assuming that @acquire@ is not already exception safe, it is /not/
-- exception-safe to pass the result of @acquire@ to @allocateTemp@: an async
-- exception could be thrown in between @acquire@ and @allocateTemp@, which
-- leaks resources.
allocateTemp :: (MonadMask m, MonadMVar m) =>
     TempRegistry m
  -> m a
  -> (a -> m ())
  -> m a
allocateTemp reg acquire free = mustBeRight <$> allocateEitherTemp reg (fmap Right acquire) free
  where
    mustBeRight :: Either Void a -> a
    mustBeRight (Left  v) = absurd v
    mustBeRight (Right a) = a

{-# SPECIALISE allocateMaybeTemp :: TempRegistry IO -> IO (Maybe a) -> (a -> IO ()) -> IO (Maybe a) #-}
allocateMaybeTemp ::
     (MonadMask m, MonadMVar m)
  => TempRegistry m
  -> m (Maybe a)
  -> (a -> m ())
  -> m (Maybe a)
allocateMaybeTemp reg acquire free = fromEither <$> allocateEitherTemp reg (toEither <$> acquire) free
  where
    toEither :: Maybe a -> Either () a
    toEither Nothing  = Left ()
    toEither (Just x) = Right x

    fromEither :: Either () a -> Maybe a
    fromEither (Left ()) = Nothing
    fromEither (Right x) = Just x

{-# SPECIALISE allocateEitherTemp :: TempRegistry IO -> IO (Either e a) -> (a -> IO ()) -> IO (Either e a) #-}
allocateEitherTemp ::
     (MonadMask m, MonadMVar m)
  => TempRegistry m
  -> m (Either e a)
  -> (a -> m ())
  -> m (Either e a)
allocateEitherTemp reg acquire free =
    mask_ $ do
      eith <- acquire
      case eith of
        Left e -> pure $ Left e
        Right x -> do
          modifyMVar_ (tempRegistryState reg) $ \st -> do
            let rid = nextId st
                rid' = rid + 1
            pure TempRegistryState {
                tempAllocated = Map.insert rid (Resource (free x)) (tempAllocated st)
              , tempFreed = tempFreed st
              , nextId = rid'
              }
          pure $ Right x

{-# SPECIALISE freeTemp :: TempRegistry IO -> IO () -> IO () #-}
-- | Do not use the resource after free.
freeTemp :: MonadMVar m => TempRegistry m -> m () -> m ()
freeTemp reg free = modifyMVarMasked_ (tempRegistryState reg) $ \st -> do
    let rid = nextId st
        rid' = rid + 1
    pure TempRegistryState {
        tempAllocated = tempAllocated st
      , tempFreed = Map.insert rid (Resource free) (tempFreed st)
      , nextId = rid'
      }

{-# SPECIALISE modifyWithTempRegistry :: IO st -> (st -> IO ()) -> (TempRegistry IO -> st -> IO (st, a)) -> IO a #-}
-- | Exception-safe modification of state with a temporary registry.
--
-- [Example:] When we modify a table's content (stored in a mutable variable),
-- we might add new runs to the levels, or remove old runs from the level. If an
-- exception is thrown before putting the updated table contents into the
-- variable, then any resources that were acquired or released in the meantime
-- should be rolled back. The 'TempRegistry' can be used to "temporarily"
-- allocate or free resources, the effects of which are rolled back in case of
-- an exception, or put into the final state when no exceptions were raised.
modifyWithTempRegistry ::
     m ~ IO
  => m st -- ^ Get the state
  -> (st -> m ()) -- ^ Store a state
  -> (TempRegistry m -> st -> m (st, a)) -- ^ Modify the state
  -> m a
modifyWithTempRegistry getSt putSt action =
    snd . fst <$> generalBracket acquire release (uncurry action)
  where
    acquire = (,) <$> newTempRegistry <*> getSt
    release (reg, oldSt) ec = do
        case ec of
          ExitCaseSuccess (newSt, _) -> putSt newSt
          ExitCaseException _        -> putSt oldSt
          ExitCaseAbort              -> putSt oldSt
        unsafeReleaseTempRegistry reg ec

{-# SPECIALISE modifyWithTempRegistry_ :: IO st -> (st -> IO ()) -> (TempRegistry IO -> st -> IO st) -> IO () #-}
-- | Like 'modifyWithTempRegistry', but without a return value.
modifyWithTempRegistry_ ::
     m ~ IO
  => m st -- ^ Get the state
  -> (st -> m ()) -- ^ Store a state
  -> (TempRegistry m -> st -> m st)
  -> m ()
modifyWithTempRegistry_ getSt putSt action =
    modifyWithTempRegistry getSt putSt (\reg content -> (,()) <$> action reg content)
