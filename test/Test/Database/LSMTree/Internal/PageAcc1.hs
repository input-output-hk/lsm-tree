{-# LANGUAGE OverloadedStrings #-}
module Test.Database.LSMTree.Internal.PageAcc1 (tests) where

import qualified Data.ByteString as BS
import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import           Database.LSMTree.Internal.Entry (Entry (..))
import           Database.LSMTree.Internal.Serialise
import qualified Database.LSMTree.Internal.Serialise.RawBytes as RB
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.RawPage

import qualified FormatPage as Proto

import           Database.LSMTree.Internal.PageAcc1

tests :: TestTree
tests = testGroup "Database.LSMTree.Internal.PageAcc1"
    [ testProperty "prototype" prototype
    , testProperty "prototypeU" prototypeU

    , testProperty "example-01a" $ prototype (Proto.Key "") (Proto.Value (BS.pack (replicate 4064 120))) Nothing
    , testProperty "example-01b" $ prototype (Proto.Key "") (Proto.Value (BS.pack (replicate 4065 120))) Nothing
    , testProperty "example-01c" $ prototype (Proto.Key "") (Proto.Value (BS.pack (replicate 4066 120))) Nothing

    , testProperty "example-02a" $ prototype (Proto.Key "") (Proto.Value (BS.pack (replicate 4050 120))) (Just (Proto.BlobRef 3 5))
    , testProperty "example-02b" $ prototype (Proto.Key "") (Proto.Value (BS.pack (replicate 4051 120))) (Just (Proto.BlobRef 3 5))
    , testProperty "example-02c" $ prototype (Proto.Key "") (Proto.Value (BS.pack (replicate 4052 120))) (Just (Proto.BlobRef 3 5))

    , testProperty "example-03a" $ prototypeU (Proto.Key "") (Proto.Value (BS.pack (replicate 4064 120)))
    , testProperty "example-03b" $ prototypeU (Proto.Key "") (Proto.Value (BS.pack (replicate 4065 120)))
    , testProperty "example-03c" $ prototypeU (Proto.Key "") (Proto.Value (BS.pack (replicate 4066 120)))
    ]

prototype
    :: Proto.Key
    -> Proto.Value
    -> Maybe Proto.BlobRef
    -> Property
prototype k v br =
    label (show (length loverflow) ++ " overflow pages") $
         propEqualRawPages lhs rhs
    .&&. counterexample "overflow pages do not match"
           (loverflow === roverflow)
  where
    (lhs, loverflow) = toRawPage $ Proto.PageLogical [(k, Proto.Insert v, br)]
    (rhs, roverflow) = singletonPage (convKey k) (convOp (Proto.Insert v) br)

prototypeU
    :: Proto.Key
    -> Proto.Value
    -> Property
prototypeU k v =
    label (show (length loverflow) ++ " overflow pages") $
         propEqualRawPages lhs rhs
    .&&. counterexample "overflow pages do not match"
           (loverflow === roverflow)
  where
    (lhs, loverflow) = toRawPage $ Proto.PageLogical [(k, Proto.Mupsert v, Nothing)]
    (rhs, roverflow) = singletonPage (convKey k) (convOp (Proto.Mupsert v) Nothing)

convKey :: Proto.Key -> SerialisedKey
convKey (Proto.Key k) = SerialisedKey $ RB.fromByteString k

convValue :: Proto.Value -> SerialisedValue
convValue (Proto.Value v) = SerialisedValue $ RB.fromByteString v

convBlobSpan :: Proto.BlobRef -> BlobSpan
convBlobSpan (Proto.BlobRef x y) = BlobSpan x y

convOp :: Proto.Operation -> Maybe Proto.BlobRef -> Entry SerialisedValue BlobSpan
convOp Proto.Delete      _            = Delete
convOp (Proto.Mupsert v) _            = Mupdate (convValue v)
convOp (Proto.Insert v)  Nothing      = Insert (convValue v)
convOp (Proto.Insert v)  (Just bspan) = InsertWithBlob (convValue v) (convBlobSpan bspan)
