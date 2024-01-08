{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | SumProd Op extended with BlobRef extraction
module Test.Database.LSMTree.Normal.StateMachine.Op (
    -- * 'Op'
    Op (..)
  , intOpId
    -- * 'HasBlobRef' class
  , HasBlobRef (..)
  ) where

import           Control.Monad ((<=<))
import           Control.Monad.IOSim (IOSim)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Data.Maybe (mapMaybe)
import           Database.LSMTree.Normal as SUT (LookupResult (..),
                     RangeLookupResult (..))
import           GHC.Show (appPrec)
import           Test.QuickCheck.StateModel.Lockstep (InterpretOp, Operation)
import qualified Test.QuickCheck.StateModel.Lockstep.Op as Op
import           Test.Util.Orphans ()
import           Test.Util.TypeFamilyWrappers (WrapBlobRef)

{-------------------------------------------------------------------------------
  'Op'
-------------------------------------------------------------------------------}

data Op a b where
  OpId                 :: Op a a
  OpFst                :: Op (a, b) a
  OpSnd                :: Op (b, a) a
  OpLeft               :: Op a (Either a b)
  OpRight              :: Op b (Either a b)
  OpFromLeft           :: Op (Either a b) a
  OpFromRight          :: Op (Either a b) b
  OpComp               :: Op b c -> Op a b -> Op a c
  OpLookupResults      :: Op [LookupResult k v (WrapBlobRef h IO blobref)] [WrapBlobRef h IO blobref]
  OpRangeLookupResults :: Op [RangeLookupResult k v (WrapBlobRef h IO blobref)] [WrapBlobRef h IO blobref]

intOpId :: Op a b -> a -> Maybe b
intOpId OpId                 = Just
intOpId OpFst                = Just . fst
intOpId OpSnd                = Just . snd
intOpId OpLeft               = Just . Left
intOpId OpRight              = Just . Right
intOpId OpFromLeft           = either Just (const Nothing)
intOpId OpFromRight          = either (const Nothing) Just
intOpId (OpComp g f)         = intOpId g <=< intOpId f
intOpId OpLookupResults      = Just . mapMaybe getBlobRef
intOpId OpRangeLookupResults = Just . mapMaybe getBlobRef

{-------------------------------------------------------------------------------
  'InterpretOp' instances
-------------------------------------------------------------------------------}

instance Operation Op where
  opIdentity = OpId

instance InterpretOp Op (Op.WrapRealized IO) where
  intOp ::
       Op a b
    -> Op.WrapRealized IO a
    -> Maybe (Op.WrapRealized IO b)
  intOp = Op.intOpRealizedId intOpId

instance InterpretOp Op (Op.WrapRealized m)
      => InterpretOp Op (Op.WrapRealized (StateT s m)) where
  intOp = Op.intOpTransformer

instance InterpretOp Op (Op.WrapRealized m)
      => InterpretOp Op (Op.WrapRealized (ReaderT r m)) where
  intOp = Op.intOpTransformer

instance InterpretOp Op (Op.WrapRealized (IOSim s)) where
  intOp ::
       Op a b
    -> Op.WrapRealized (IOSim s) a
    -> Maybe (Op.WrapRealized (IOSim s) b)
  intOp = \case
      OpId                 -> Just
      OpFst                -> Just . Op.WrapRealized . fst . Op.unwrapRealized
      OpSnd                -> Just . Op.WrapRealized . snd . Op.unwrapRealized
      OpLeft               -> Just . Op.WrapRealized . Left . Op.unwrapRealized
      OpRight              -> Just . Op.WrapRealized . Right . Op.unwrapRealized
      OpFromLeft           -> either (Just . Op.WrapRealized) (const Nothing) . Op.unwrapRealized
      OpFromRight          -> either (const Nothing) (Just . Op.WrapRealized) . Op.unwrapRealized
      OpComp g f           -> Op.intOp g <=< Op.intOp f
      OpLookupResults      -> Just  . Op.WrapRealized . mapMaybe getBlobRef . Op.unwrapRealized
      OpRangeLookupResults -> Just . Op.WrapRealized . mapMaybe getBlobRef . Op.unwrapRealized

{-------------------------------------------------------------------------------
  'Show' and 'Eq' instances
-------------------------------------------------------------------------------}

sameOp :: Op a b -> Op c d -> Bool
sameOp = go
  where
    go :: Op a b -> Op c d -> Bool
    go OpId                 OpId                 = True
    go OpFst                OpFst                = True
    go OpSnd                OpSnd                = True
    go OpLeft               OpLeft               = True
    go OpRight              OpRight              = True
    go OpFromLeft           OpFromLeft           = True
    go OpFromRight          OpFromRight          = True
    go (OpComp g f)         (OpComp g' f')       = go g g' && go f f'
    go OpLookupResults      OpLookupResults      = True
    go OpRangeLookupResults OpRangeLookupResults = True
    go _                    _                    = False

    _coveredAllCases :: Op a b -> ()
    _coveredAllCases = \case
        OpId                   -> ()
        OpFst                  -> ()
        OpSnd                  -> ()
        OpLeft                 -> ()
        OpRight                -> ()
        OpFromLeft             -> ()
        OpFromRight            -> ()
        OpComp{}               -> ()
        OpLookupResults{}      -> ()
        OpRangeLookupResults{} -> ()


instance Eq (Op a b)  where
  (==) = sameOp

-- TODO: parentheses
instance Show (Op a b) where
  showsPrec :: Int -> Op a b -> ShowS
  showsPrec p = \op -> case op of
      OpComp{} -> showParen (p > appPrec) (go op)
      _        -> go op
    where
      go :: Op x y -> String -> String
      go OpId                 = showString "id"
      go OpFst                = showString "fst"
      go OpSnd                = showString "snd"
      go OpLeft               = showString "Left"
      go OpRight              = showString "Right"
      go OpFromLeft           = showString "FromLeft"
      go OpFromRight          = showString "FromRight"
      go (OpComp g f)         = go g . showString " . " . go f
      go OpLookupResults      = showString "mapMaybe getBlobRef"
      go OpRangeLookupResults = showString "mapMaybe getBlobRef"

{-------------------------------------------------------------------------------
  'HasBlobRef' class
-------------------------------------------------------------------------------}

class HasBlobRef f where
  getBlobRef :: f blobref  -> Maybe blobref

instance HasBlobRef (LookupResult k v) where
  getBlobRef NotFound{}                  = Nothing
  getBlobRef Found{}                     = Nothing
  getBlobRef (FoundWithBlob _ _ blobref) = Just blobref

instance HasBlobRef (RangeLookupResult k v) where
  getBlobRef FoundInRange{}                     = Nothing
  getBlobRef (FoundInRangeWithBlob _ _ blobref) = Just blobref
