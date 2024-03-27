{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Database.LSMTree.Generators (
    tests
  , prop_arbitraryAndShrinkPreserveInvariant
  , prop_forAllArbitraryAndShrinkPreserveInvariant
  , deepseqInvariant
  ) where

import           Control.DeepSeq (NFData, deepseq)
import           Data.ByteString (ByteString)
import qualified Data.Vector.Primitive as PV
import           Data.Word (Word64, Word8)

import           Database.LSMTree.Generators
import           Database.LSMTree.Internal.Serialise.RawBytes (RawBytes (..))

import           Test.Database.LSMTree.Internal.Run.Index.Compact ()
import           Test.QuickCheck (Arbitrary (..), Gen, Testable (..),
                     forAllShrink)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Test.Database.LSMTree.Generators" [
      testGroup "WriteBuffer" $
        prop_arbitraryAndShrinkPreserveInvariant (writeBufferInvariant @ByteString @ByteString @ByteString)
    , testGroup "Range-finder bit-precision" $
        prop_arbitraryAndShrinkPreserveInvariant rfprecInvariant
    , testGroup "LogicalPageSummaries" $
        prop_arbitraryAndShrinkPreserveInvariant (pagesInvariant @Word64)
    , testGroup "Chunk size" $
        prop_arbitraryAndShrinkPreserveInvariant chunkSizeInvariant
    , testGroup "Raw bytes" $
        [testProperty "packRawBytesPinnedOrUnpinned"
                      prop_packRawBytesPinnedOrUnpinned
        ]
     ++ prop_arbitraryAndShrinkPreserveInvariant (deepseqInvariant @RawBytes)
    ]

prop_arbitraryAndShrinkPreserveInvariant ::
    forall a. (Arbitrary a, Show a) => (a -> Bool) -> [TestTree]
prop_arbitraryAndShrinkPreserveInvariant =
    prop_forAllArbitraryAndShrinkPreserveInvariant arbitrary shrink

prop_forAllArbitraryAndShrinkPreserveInvariant ::
    forall a. Show a => Gen a -> (a -> [a]) -> (a -> Bool) -> [TestTree]
prop_forAllArbitraryAndShrinkPreserveInvariant gen shr inv =
    [ testProperty "Arbitrary satisfies invariant" $
            property $ forAllShrink gen shr inv
    , testProperty "Shrinking satisfies invariant" $
            property $ forAllShrink gen shr (all inv . shr)
    ]

-- | Trivial invariant, but checks that the value is finite
deepseqInvariant :: NFData a => a -> Bool
deepseqInvariant x = x `deepseq` True

prop_packRawBytesPinnedOrUnpinned :: Bool -> [Word8] -> Bool
prop_packRawBytesPinnedOrUnpinned pinned ws =
    packRawBytesPinnedOrUnpinned pinned ws == RawBytes (PV.fromList ws)
