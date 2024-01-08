{-# LANGUAGE StandaloneKindSignatures #-}

module Database.LSMTree.Internal.BlobRef (
    BlobRef (..),
) where

import           Data.Kind (Type)
import           Data.Word

-- | A handle-like reference to an on-disk blob.
--
-- The blob can be retrieved based on the reference. Though blob references are
-- handle-like, they /do not/ keep open files. This means that, when a blob
-- reference is returned by a lookup, any updates (e.g., close, inserts,
-- deletes, mupserts) to the corresponding table handle from then onward may
-- cause the blob reference to be invalidated (e.g., the blob has gone missing
-- because the blob file was removed). An invalidated blob reference will throw
-- an error. The only guarantee is this: as long as the table handle that the
-- blob reference originated from is not updated, the blob reference will be
-- valid.
--
-- Blob comes from the acronym __Binary Large OBject (BLOB)__ and in many
-- database implementations refers to binary data that is larger than usual
-- values and is handled specially. In our context we will allow optionally a
-- blob associated with each value in the table.
type BlobRef :: (Type -> Type) -> Type -> Type
data BlobRef m blob = BlobRef {
      -- | TODO: fill in with an actual handle or filename type.
      blobFile      :: !()
    , blobRefOffset :: !Word64
    , blobRefSize   :: !Word32
    }
