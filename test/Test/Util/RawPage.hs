module Test.Util.RawPage (
    toRawPage,
    assertEqualRawPages,
    propEqualRawPages,
) where

import           Control.Monad (unless)
import           Data.Align (align)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.List.Split (chunksOf)
import           Data.Primitive.ByteArray (ByteArray (..))
import           Data.These (These (..))
import           Data.Word (Word8)
import           Database.LSMTree.Internal.BitMath (div16, mod16)
import           Database.LSMTree.Internal.RawOverflowPage (RawOverflowPage,
                     makeRawOverflowPage)
import           Database.LSMTree.Internal.RawPage (RawPage, makeRawPage,
                     rawPageRawBytes)
import qualified Database.LSMTree.Internal.Serialise.RawBytes as RB
import           FormatPage (PageLogical, encodePage, serialisePage)
import qualified System.Console.ANSI as ANSI
import           Test.Tasty.HUnit (Assertion, assertFailure)
import           Test.Tasty.QuickCheck (Property, counterexample)

-- | Convert prototype 'PageLogical' to 'RawPage'.
toRawPage :: PageLogical -> (RawPage, [RawOverflowPage])
toRawPage p = (page, overflowPages)
  where
    bs = serialisePage $ encodePage p
    (pfx, sfx) = BS.splitAt 4096 bs -- hardcoded page size.
    page          = makeRawPageBS pfx
    overflowPages = [ makeRawOverflowPageBS sfxpg
                    | sfxpg <- takeWhile (not . BS.null)
                                 [ BS.drop n sfx | n <- [0, 4096 .. ] ]
                    ]

makeRawPageBS :: BS.ByteString -> RawPage
makeRawPageBS bs =
    case SBS.toShort bs of
      SBS.SBS ba -> makeRawPage (ByteArray ba) 0

makeRawOverflowPageBS :: BS.ByteString -> RawOverflowPage
makeRawOverflowPageBS bs =
    case SBS.toShort bs of
      SBS.SBS ba -> makeRawOverflowPage (ByteArray ba) 0 (BS.length bs)

assertEqualRawPages :: RawPage -> RawPage -> Assertion
assertEqualRawPages a b = unless (a == b) $ do
    assertFailure $ "unequal pages:\n" ++ ANSI.setSGRCode [ANSI.Reset] ++ compareBytes (RB.unpack (rawPageRawBytes a)) (RB.unpack (rawPageRawBytes b))

propEqualRawPages :: RawPage -> RawPage -> Property
propEqualRawPages a b = counterexample
    (ANSI.setSGRCode [ANSI.Reset] ++ compareBytes (RB.unpack (rawPageRawBytes a)) (RB.unpack (rawPageRawBytes b)))
    (a == b)

-- Print two bytestreams next to each other highlighting the differences.
compareBytes :: [Word8] -> [Word8] -> String
compareBytes xs ys = unlines $ go (grouping chunks)
  where
    go :: [Either [()] [([Word8], [Word8])]] -> [String]
    go []                = []
    go (Left _ : zs)     = "..." : go zs
    go (Right diff : zs) = map (uncurry showDiff) diff ++ go zs

    showDiff :: [Word8] -> [Word8] -> String
    showDiff = aux id id where
        aux :: ShowS -> ShowS ->  [Word8] -> [Word8] -> String
        aux accl accr []     []     = accl . showString "  " . accr $ ""
        aux accl accr []     rs     = accl . showString "  " . accr . green (concatMapS showsWord8 rs) $ ""
        aux accl accr ls     []     = accl . red (concatMapS showsWord8 ls) . showString "  " . accr $ ""
        aux accl accr (l:ls) (r:rs)
            | l == r                = aux (accl . showsWord8 l) (accr . showsWord8 r) ls rs
            | otherwise             = aux (accl . red (showsWord8 l)) (accr . green (showsWord8 r)) ls rs

    -- chunks are either equal, or not
    chunks :: [Either () ([Word8], [Word8])]
    chunks =
        [ case b of
            These x y
                | x == y    -> Left  ()
                | otherwise -> Right (x, y)
            This x -> Right (x, [])
            That y -> Right ([], y)
        | b <- align (chunksOf 16 xs) (chunksOf 16 ys)
        ]

sgr :: [ANSI.SGR] -> ShowS
sgr cs = (ANSI.setSGRCode cs ++)

red :: ShowS -> ShowS
red s = sgr [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red] . s . sgr [ANSI.Reset]

green :: ShowS -> ShowS
green s = sgr [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green] . s . sgr [ANSI.Reset]

concatMapS :: (a -> ShowS) -> [a] -> ShowS
concatMapS f xs = \acc -> foldr (\a acc' -> f a acc') acc xs

showsWord8 :: Word8 -> ShowS
showsWord8 w = \acc -> hexdigit (div16 w) : hexdigit (mod16 w) : acc

grouping :: [Either a b] -> [Either [a] [b]]
grouping = foldr add []
  where
    add (Left x)       []                = [Left [x]]
    add (Right y)      []                = [Right [y]]
    add (Left x)       (Left xs  : rest) = Left (x : xs) : rest
    add (Right y) rest@(Left _   : _)    = Right [y] : rest
    add (Left x)  rest@(Right _  : _ )   = Left [x] : rest
    add (Right y)      (Right ys : rest) = Right (y:ys) : rest


hexdigit :: (Num a, Eq a) => a -> Char
hexdigit 0x0 = '0'
hexdigit 0x1 = '1'
hexdigit 0x2 = '2'
hexdigit 0x3 = '3'
hexdigit 0x4 = '4'
hexdigit 0x5 = '5'
hexdigit 0x6 = '6'
hexdigit 0x7 = '7'
hexdigit 0x8 = '8'
hexdigit 0x9 = '9'
hexdigit 0xA = 'a'
hexdigit 0xB = 'b'
hexdigit 0xC = 'c'
hexdigit 0xD = 'd'
hexdigit 0xE = 'e'
hexdigit 0xF = 'f'
hexdigit _   = '?'
