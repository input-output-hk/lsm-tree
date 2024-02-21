{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

module Database.LSMTree.Internal.Entry (
    Entry (..)
  , onValue
  , onBlobRef
  , NumEntries (..)
    -- * Injections/projections
  , updateToEntryNormal
  , updateToEntryMonoidal
    -- * Value resolution/merging
  , combine
  , combinesMonoidal
  , combinesMonoidal'
  , combinesNormal
  , combinesNormal'
  , resolveEntriesNormal
  , resolveEntriesMonoidal
  ) where

import           Control.DeepSeq (NFData (..))
import           Data.Bifoldable (Bifoldable (..))
import           Data.Bifunctor (Bifunctor (..))
import           Data.Foldable1 (Foldable1)
import qualified Data.Foldable1 as Foldable1
import           Data.List.NonEmpty
import           Data.Semigroup
import qualified Database.LSMTree.Internal.Monoidal as Monoidal
import qualified Database.LSMTree.Internal.Normal as Normal

data Entry v blobref
    = Insert !v
    | InsertWithBlob !v !blobref
    | Mupdate !v
    | Delete
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (NFData v, NFData blobref) => NFData (Entry v blobref) where
    rnf (Insert v)            = rnf v
    rnf (InsertWithBlob v br) = rnf v `seq` rnf br
    rnf (Mupdate v)           = rnf v
    rnf Delete                = ()

onValue :: v' -> (v -> v') -> Entry v blobref -> v'
onValue def f = \case
    Insert v           -> f v
    InsertWithBlob v _ -> f v
    Mupdate v          -> f v
    Delete             -> def

onBlobRef :: blobref' -> (blobref -> blobref') -> Entry v blobref -> blobref'
onBlobRef def g = \case
    Insert{}            -> def
    InsertWithBlob _ br -> g br
    Mupdate{}           -> def
    Delete              -> def

instance Bifunctor Entry where
  first f = \case
      Insert v            -> Insert (f v)
      InsertWithBlob v br -> InsertWithBlob (f v) br
      Mupdate v           -> Mupdate (f v)
      Delete              -> Delete

  second g = \case
      Insert v            -> Insert v
      InsertWithBlob v br -> InsertWithBlob v (g br)
      Mupdate v           -> Mupdate v
      Delete              -> Delete

instance Bifoldable Entry where
  bifoldMap f g = \case
      Insert v            -> f v
      InsertWithBlob v br -> f v <> g br
      Mupdate v           -> f v
      Delete              -> mempty


newtype NumEntries = NumEntries { unNumEntries :: Int }
  deriving (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  Injections/projections
-------------------------------------------------------------------------------}

updateToEntryNormal :: Normal.Update v blob -> Entry v blob
updateToEntryNormal = \case
    Normal.Insert v Nothing  -> Insert v
    Normal.Insert v (Just b) -> InsertWithBlob v b
    Normal.Delete            -> Delete

entryToUpdateNormal :: Entry v blob -> Maybe (Normal.Update v blob)
entryToUpdateNormal = \case
    Insert v           -> Just (Normal.Insert v Nothing)
    InsertWithBlob v b -> Just (Normal.Insert v (Just b))
    Mupdate _          -> Nothing
    Delete             -> Just Normal.Delete

updateToEntryMonoidal :: Monoidal.Update v -> Entry v blob
updateToEntryMonoidal = \case
    Monoidal.Insert v  -> Insert v
    Monoidal.Mupsert v -> Mupdate v
    Monoidal.Delete    -> Delete

entryToUpdateMonoidal :: Entry v blob -> Maybe (Monoidal.Update v)
entryToUpdateMonoidal = \case
    Insert v           -> Just (Monoidal.Insert v)
    InsertWithBlob _ _ -> Nothing
    Mupdate v          -> Just (Monoidal.Mupsert v)
    Delete             -> Just Monoidal.Delete

{-------------------------------------------------------------------------------
  Value resolution/merging
-------------------------------------------------------------------------------}

-- | Given a value-merge function, combine entries
combine :: (v -> v -> v) -> Entry v blobref -> Entry v blobref -> Entry v blobref
combine _ e@Delete            _                       = e
combine _ e@Insert {}         _                       = e
combine _ e@InsertWithBlob {} _                       = e
combine _   (Mupdate u)       Delete                  = Insert u
combine f   (Mupdate u)       (Insert v)              = Insert (f u v)
combine f   (Mupdate u)       (InsertWithBlob v blob) = InsertWithBlob (f u v) blob
combine f   (Mupdate u)       (Mupdate v)             = Mupdate (f u v)

-- | Like 'combine', but the entries are inside a 'Maybe' (semigroup)
combineMaybe ::
     (v -> v -> v)
  -> Maybe (Entry v blobref)
  -> Maybe (Entry v blobref)
  -> Maybe (Entry v blobref)
combineMaybe _ Nothing  b        = b
combineMaybe _ a        Nothing  = a
combineMaybe f (Just a) (Just b) = Just (combine f a b)

{-# SPECIALIZE combinesMonoidal :: (v -> v -> v) -> NonEmpty (Entry v blob) -> Entry v blob #-}
combinesMonoidal ::
     Foldable1 f
  => (v -> v -> v)
  -> f (Entry v blob)
  -> Entry v blob
combinesMonoidal f = foldr1 (combine f) -- short-circuit fold

{-# SPECIALIZE combinesMonoidal' :: (v -> v -> v) -> NonEmpty (Maybe (Entry v blob)) -> Maybe (Entry v blob) #-}
combinesMonoidal' ::
     Foldable1 f
  => (v -> v -> v)
  -> f (Maybe (Entry v blob))
  -> Maybe (Entry v blob)
combinesMonoidal' f = foldr1 (combineMaybe f) -- short-circuit fold

{-# SPECIALIZE combinesNormal :: NonEmpty (Entry v blob) -> Entry v blob #-}
combinesNormal :: Foldable1 f => f (Entry v blob) -> Entry v blob
combinesNormal = getFirst . Foldable1.foldMap1 First

{-# SPECIALIZE combinesNormal' :: NonEmpty (Maybe (Entry v blob)) -> Maybe (Entry v blob) #-}
combinesNormal' :: Foldable1 f => f (Maybe (Entry v blob)) -> Maybe (Entry v blob)
combinesNormal' = fmap getFirst . Foldable1.foldMap1 (fmap First)

{-# SPECIALIZE resolveEntriesNormal :: NonEmpty (Entry v blob) -> Maybe (Normal.Update v blob) #-}
-- | Returns 'Nothing' if the combined entries can not be mapped to an
-- 'Normal.Update'.
resolveEntriesNormal ::
     Foldable1 f
  => f (Entry v blob)
  -> Maybe (Normal.Update v blob)
resolveEntriesNormal es = entryToUpdateNormal (combinesNormal es)

{-# SPECIALIZE resolveEntriesMonoidal :: (v -> v -> v) -> NonEmpty (Entry v blob) -> Maybe (Monoidal.Update v) #-}
-- | Returns 'Nothing' if the combined entries can not be mapped to an
-- 'Monoidal.Update'.
resolveEntriesMonoidal ::
     Foldable1 f
  => (v -> v -> v)
  -> f (Entry v blob)
  -> Maybe (Monoidal.Update v)
resolveEntriesMonoidal f es = entryToUpdateMonoidal (combinesMonoidal f es)
