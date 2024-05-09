-- | Tests for the @lsm-tree@ library.
--
module Main (main) where

import qualified Test.Database.LSMTree.Generators
import qualified Test.Database.LSMTree.Internal.BloomFilter
import qualified Test.Database.LSMTree.Internal.Entry
import qualified Test.Database.LSMTree.Internal.IndexCompact
import qualified Test.Database.LSMTree.Internal.Lookup
import qualified Test.Database.LSMTree.Internal.Merge
import qualified Test.Database.LSMTree.Internal.Merge.Readers
import qualified Test.Database.LSMTree.Internal.Monkey
import qualified Test.Database.LSMTree.Internal.PageAcc
import qualified Test.Database.LSMTree.Internal.PageAcc1
import qualified Test.Database.LSMTree.Internal.RawOverflowPage
import qualified Test.Database.LSMTree.Internal.RawPage
import qualified Test.Database.LSMTree.Internal.Run
import qualified Test.Database.LSMTree.Internal.RunAcc
import qualified Test.Database.LSMTree.Internal.Serialise
import qualified Test.Database.LSMTree.Internal.Serialise.Class
import qualified Test.Database.LSMTree.Model.Monoidal
import qualified Test.Database.LSMTree.Model.Normal
import qualified Test.Database.LSMTree.ModelIO.Monoidal
import qualified Test.Database.LSMTree.ModelIO.Normal
import qualified Test.Database.LSMTree.Normal.StateMachine
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "lsm-tree"
    [ Test.Database.LSMTree.Generators.tests
    , Test.Database.LSMTree.Internal.BloomFilter.tests
    , Test.Database.LSMTree.Internal.Entry.tests
    , Test.Database.LSMTree.Internal.Lookup.tests
    , Test.Database.LSMTree.Internal.Merge.tests
    , Test.Database.LSMTree.Internal.Merge.Readers.tests
    , Test.Database.LSMTree.Internal.Monkey.tests
    , Test.Database.LSMTree.Internal.PageAcc.tests
    , Test.Database.LSMTree.Internal.PageAcc1.tests
    , Test.Database.LSMTree.Internal.RawPage.tests
    , Test.Database.LSMTree.Internal.RawOverflowPage.tests
    , Test.Database.LSMTree.Internal.Run.tests
    , Test.Database.LSMTree.Internal.RunAcc.tests
    , Test.Database.LSMTree.Internal.IndexCompact.tests
    , Test.Database.LSMTree.Internal.Serialise.tests
    , Test.Database.LSMTree.Internal.Serialise.Class.tests
    , Test.Database.LSMTree.Model.Normal.tests
    , Test.Database.LSMTree.Model.Monoidal.tests
    , Test.Database.LSMTree.ModelIO.Normal.tests
    , Test.Database.LSMTree.ModelIO.Monoidal.tests
    , Test.Database.LSMTree.Normal.StateMachine.tests
    ]
