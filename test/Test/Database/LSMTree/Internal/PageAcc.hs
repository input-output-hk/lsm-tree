{-# LANGUAGE OverloadedStrings #-}
module Test.Database.LSMTree.Internal.PageAcc (tests) where

import           Control.Monad.ST.Strict (runST)
import qualified Data.ByteString as BS

import           Database.LSMTree.Internal.BlobRef (BlobSpan (..))
import           Database.LSMTree.Internal.Entry (Entry (..))
import           Database.LSMTree.Internal.PageAcc
import           Database.LSMTree.Internal.RawPage (RawPage)
import           Database.LSMTree.Internal.Serialise

import qualified Database.LSMTree.Extras.ReferenceImpl as Ref
import           Test.Util.RawPage (propEqualRawPages)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Database.LSMTree.Internal.PageAcc" $
    [ testProperty "vs reference impl" prop_vsReferenceImpl
    , testProperty "maxPageKeys == 759" (maxPageKeys === 759)
    ]

 ++ [ testProperty
        ("example-" ++ show (n :: Int) ++ [ a | length exs > 1 ])
        (prop_vsReferenceImpl (Ref.PageContentMaybeOverfull kops))
    | (n, exs)  <- zip [0..] examples
    , (a, kops) <- zip ['a'..] exs
    ]

  where
    examples = example0123 ++ [example4s, example5s, example6s, example7s]

    example0123 =
      map (:[])
      [ []
      , [(Ref.Key "foobar", Ref.Delete)]
      , [(Ref.Key "foobar", Ref.Insert (Ref.Value "value")
                                       (Just (Ref.BlobRef 111 333)))]
      , [(Ref.Key "\NUL", Ref.Delete), (Ref.Key "\SOH", Ref.Delete)]
      ]

    example4s = [ [(Ref.Key "", Ref.Insert (Ref.Value (BS.replicate sz 120))
                                           Nothing)]
                | sz <- [4063..4065] ]

    example5s = [ [ (Ref.Key "",Ref.Delete)
                  , (Ref.Key "k", Ref.Insert (Ref.Value (BS.replicate sz 120))
                                             Nothing) ]
                | sz <- [4060..4062] ]

    example6s = [ [(Ref.Key "", Ref.Insert (Ref.Value (BS.replicate sz 120))
                                           (Just (Ref.BlobRef 111 333))) ]
                | sz <- [4051..4053] ]

    example7s = [ (replicate maxPageKeys     (Ref.Key " ",Ref.Delete))
                , (replicate (maxPageKeys+1) (Ref.Key " ",Ref.Delete))
                , (replicate (maxPageKeys+1) (Ref.Key "", Ref.Delete))
                ]

maxPageKeys :: Int
maxPageKeys =
    go 0 (Ref.pageSizeEmpty Ref.DiskPage4k)
  where
    go s ps =
      case Ref.pageSizeAddElem (Ref.Key " ", Ref.Delete) ps of
        Nothing  -> s
        Just ps' -> go (s + 1) ps'

prop_vsReferenceImpl :: Ref.PageContentMaybeOverfull -> Property
prop_vsReferenceImpl (Ref.PageContentMaybeOverfull kops) =
    case (refImpl, realImpl) of
      (Just (lhs, _), Just rhs) -> propEqualRawPages lhs rhs
      (Nothing,       Nothing)  -> label "overflow" $
                                   property True

      -- Special cases where we allow a test pass.
      (Just _,        Nothing)
        -- The PageAcc does not support single-key/op pairs that overflow onto
        -- multiple pages. That case is handled by PageAcc1.
        | [_]       <- kops
        , Just page <- Ref.encodePage Ref.DiskPage4k kops
        , Ref.pageDiskPages page > 1
                                -> label "PageAcc1 special case" $
                                   property True

        -- PageAcc (quite reasonably) assumes that keys are not all empty
        -- (since in practice they'll be distinct) and thus it can impose an
        -- upper bound on the number of keys in a page. It's possible to
        -- construct test cases with empty keys that exceed the buffer size.
        | length kops >= maxPageKeys
                                -> label "max number of keys reached" $
                                   property True

      _                         -> property False
  where
    refImpl  = Ref.toRawPageMaybeOverfull (Ref.PageContentMaybeOverfull kops)
    realImpl = toRawPageViaPageAcc [ (Ref.toSerialisedKey k, Ref.toEntry op)
                                   | (k,op) <- kops ]

-- | Use a 'PageAcc' to try to make a 'RawPage' from key\/op pairs. It will
-- return @Nothing@ if the key\/op pairs would not all fit in a page.
--
toRawPageViaPageAcc :: [(SerialisedKey, Entry SerialisedValue BlobSpan)]
                    -> Maybe RawPage
toRawPageViaPageAcc kops0 =
    runST $ do
      acc <- newPageAcc
      go acc kops0
  where
    go acc []            = Just <$> serialisePageAcc acc
    go acc ((k,op):kops) = do
      added <- pageAccAddElem acc k op
      if added
        then go acc kops
        else return Nothing

