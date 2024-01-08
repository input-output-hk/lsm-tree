{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Database.LSMTree.Model.Normal (
    -- * Temporary placeholder types
    SomeSerialisationConstraint (..)
    -- * Tables
  , Table
  , empty
    -- * Table querying and updates
    -- ** Queries
  , Range (..)
  , LookupResult (..)
  , lookups
  , RangeLookupResult (..)
  , rangeLookup
    -- ** Updates
  , Update (..)
  , updates
  , inserts
  , deletes
    -- ** Blobs
  , BlobRef
  , retrieveBlobs
    -- * Snapshots
  , snapshot
    -- * Multiple writable table handles
  , duplicate
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import           Data.Foldable (foldl')
import           Data.Map (Map)
import qualified Data.Map.Range as Map.R
import qualified Data.Map.Strict as Map
import           Database.LSMTree.Common (Range (..),
                     SomeSerialisationConstraint (..))
import           Database.LSMTree.Normal (LookupResult (..),
                     RangeLookupResult (..), Update (..))
import           GHC.Exts (IsList (..))

{-------------------------------------------------------------------------------
  Tables
-------------------------------------------------------------------------------}

data Table k v blob = Table
    { _values :: Map BS.ByteString (BS.ByteString, Maybe (BlobRef blob))
    }

type role Table nominal nominal nominal

-- | An empty table.
empty :: Table k v blob
empty = Table Map.empty

-- | This instance is for testing and debugging only.
instance
    ( SomeSerialisationConstraint k
    , SomeSerialisationConstraint v
    , SomeSerialisationConstraint blob
    ) => IsList (Table k v blob)
  where
    type Item (Table k v blob) = (k, v, Maybe blob)
    fromList xs = Table $ Map.fromList
        [ (serialise k, (serialise v, mkBlobRef <$> mblob))
        | (k, v, mblob) <- xs
        ]

    toList (Table m) =
        [ (deserialise k, deserialise v, getBlobFromRef <$> mbref)
        | (k, (v, mbref)) <- Map.toList m
        ]

-- | This instance is for testing and debugging only.
instance Show (Table k v blob) where
    showsPrec d (Table tbl) = showParen (d > 10)
        $ showString "fromList "
        . showsPrec 11 (toList (Table @BS.ByteString @BS.ByteString @BS.ByteString tbl'))
      where
        tbl' :: Map BS.ByteString (BS.ByteString, Maybe (BlobRef BS.ByteString))
        tbl' = fmap (fmap (fmap coerceBlobRef)) tbl

-- | This instance is for testing and debugging only.
deriving instance Eq (Table k v blob)

{-------------------------------------------------------------------------------
  Table querying and updates
-------------------------------------------------------------------------------}

-- | Perform a batch of lookups.
--
-- Lookups can be performed concurrently from multiple Haskell threads.
lookups ::
    (SomeSerialisationConstraint k, SomeSerialisationConstraint v)
  => [k]
  -> Table k v blob
  -> [LookupResult k v (BlobRef blob)]
lookups ks tbl =
    [ case Map.lookup (serialise k) (_values tbl) of
        Nothing           -> NotFound k
        Just (v, Nothing) -> Found k (deserialise v)
        Just (v, Just br) -> FoundWithBlob k (deserialise v) br
    | k <- ks
    ]

-- | Perform a range lookup.
--
-- Range lookups can be performed concurrently from multiple Haskell threads.
rangeLookup :: forall k v blob.
     (SomeSerialisationConstraint k, SomeSerialisationConstraint v)
  => Range k
  -> Table k v blob
  -> [RangeLookupResult k v (BlobRef blob)]
rangeLookup r tbl =
    [ case v of
        (v', Nothing) -> FoundInRange (deserialise k) (deserialise v')
        (v', Just br) -> FoundInRangeWithBlob (deserialise k) (deserialise v') br
    | let (lb, ub) = convertRange r
    , (k, v) <- Map.R.rangeLookup lb ub (_values tbl)
    ]
  where
    convertRange :: Range k -> (Map.R.Bound BS.ByteString, Map.R.Bound BS.ByteString)
    convertRange (FromToExcluding lb ub) =
        ( Map.R.Bound (serialise lb) Map.R.Inclusive
        , Map.R.Bound (serialise ub) Map.R.Exclusive )
    convertRange (FromToIncluding lb ub) =
        ( Map.R.Bound (serialise lb) Map.R.Inclusive
        , Map.R.Bound (serialise ub) Map.R.Inclusive )

-- | Perform a mixed batch of inserts and deletes.
--
-- Updates can be performed concurrently from multiple Haskell threads.
updates :: forall k v blob.
     ( SomeSerialisationConstraint k
     , SomeSerialisationConstraint v
     , SomeSerialisationConstraint blob
     )
  => [(k, Update v blob)]
  -> Table k v blob
  -> Table k v blob
updates ups tbl0 = foldl' update tbl0 ups where
    update :: Table k v blob -> (k, Update v blob) -> Table k v blob
    update tbl (k, Delete) = tbl
        { _values = Map.delete (serialise k) (_values tbl) }
    update tbl (k, Insert v Nothing) = tbl
        { _values = Map.insert (serialise k) (serialise v, Nothing) (_values tbl) }
    update tbl (k, Insert v (Just blob)) = tbl
        { _values = Map.insert (serialise k) (serialise v, Just (mkBlobRef blob)) (_values tbl)
        }

-- | Perform a batch of inserts.
--
-- Inserts can be performed concurrently from multiple Haskell threads.
inserts ::
     ( SomeSerialisationConstraint k
     , SomeSerialisationConstraint v
     , SomeSerialisationConstraint blob
     )
  => [(k, v, Maybe blob)]
  -> Table k v blob
  -> Table k v blob
inserts = updates . fmap (\(k, v, blob) -> (k, Insert v blob))

-- | Perform a batch of deletes.
--
-- Deletes can be performed concurrently from multiple Haskell threads.
deletes ::
     ( SomeSerialisationConstraint k
     , SomeSerialisationConstraint v
     , SomeSerialisationConstraint blob
     )
  => [k]
  -> Table k v blob
  -> Table k v blob
deletes = updates . fmap (,Delete)

-- | A reference to an on-disk blob.
--
-- The blob can be retrieved based on the reference.
--
-- Blob comes from the acronym __Binary Large OBject (BLOB)__ and in many
-- database implementations refers to binary data that is larger than usual
-- values and is handled specially. In our context we will allow optionally a
-- blob associated with each value in the table.
data BlobRef blob = BlobRef
    !BS.ByteString  -- ^ digest
    !BS.ByteString  -- ^ actual contents
  deriving (Show)

type role BlobRef nominal

mkBlobRef :: SomeSerialisationConstraint blob => blob -> BlobRef blob
mkBlobRef blob =  BlobRef (SHA256.hash bs) bs
  where
    !bs = serialise blob

coerceBlobRef :: BlobRef blob -> BlobRef blob'
coerceBlobRef (BlobRef d b) = BlobRef d b

getBlobFromRef :: SomeSerialisationConstraint blob => BlobRef blob -> blob
getBlobFromRef (BlobRef _ bs) = deserialise bs

instance Eq (BlobRef blob) where
    BlobRef x _ == BlobRef y _ = x == y

-- | Perform a batch of blob retrievals.
--
-- This is a separate step from 'lookups' and 'rangeLookups. The result of a
-- lookup can include a 'BlobRef', which can be used to retrieve the actual
-- 'Blob'.
--
-- Blob lookups can be performed concurrently from multiple Haskell threads.
retrieveBlobs ::
     (SomeSerialisationConstraint blob)
  => [BlobRef blob]
  -> [blob]
retrieveBlobs refs = map getBlobFromRef refs

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

snapshot ::
     Table k v blob
  -> Table k v blob
snapshot = id

{-------------------------------------------------------------------------------
  Mutiple writable table handles
-------------------------------------------------------------------------------}

duplicate ::
     Table k v blob
  -> Table k v blob
duplicate = id
