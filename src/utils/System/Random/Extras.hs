{-# LANGUAGE BangPatterns #-}

module System.Random.Extras (
    -- * Sampling from uniform distributions
    uniformWithoutReplacement
  , uniformWithoutReplacement'
  , uniformWithReplacement
  , sampleUniformWithoutReplacement
  , sampleUniformWithReplacement
  ) where

import           Data.List (unfoldr)
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.Random (StdGen, Uniform, uniform, uniformR)
import           Text.Printf (printf)

{-------------------------------------------------------------------------------
  Sampling from uniform distributions
-------------------------------------------------------------------------------}

uniformWithoutReplacement :: (Ord a, Uniform a) => StdGen -> Int -> [a]
uniformWithoutReplacement rng0 n0 = take n0 $
    go Set.empty rng0
  where
    go !seen !rng
        | Set.member x seen =     go               seen  rng'
        | otherwise         = x : go (Set.insert x seen) rng'
      where
        (!x, !rng') = uniform rng

-- | Like 'uniformWithoutReplacement', but returns a 'Set' instead.
--
-- Note: does not benefit from laziness, since all values must be generated
-- before the result can be returned.
uniformWithoutReplacement' :: (Ord a, Uniform a) => StdGen -> Int -> Set a
uniformWithoutReplacement' rng0 n0 =
    go Set.empty (0 :: Int) rng0
  where
    go !seen !n !rng
        | n == n0           = seen
        | Set.member x seen = go               seen  n     rng'
        | otherwise         = go (Set.insert x seen) (n+1) rng'
      where
        (!x, !rng') = uniform rng

uniformWithReplacement :: Uniform a => StdGen -> Int -> [a]
uniformWithReplacement rng0 n0 = take n0 $
    unfoldr (Just . uniform) rng0

sampleUniformWithoutReplacement :: Ord a => StdGen -> Int -> [a] -> [a]
sampleUniformWithoutReplacement rng0 n xs0 = take n $
    go (Set.fromList xs0) rng0
  where
    go !xs !_rng | Set.null xs = error $
        printf "sampleUniformWithoutReplacement: n > length xs0 for n=%d, \
               \ length xs0=%d"
               n
               (length xs0)

    go !xs !rng = x : go xs' rng'
      where
        (i, rng') = uniformR (0, Set.size xs - 1) rng
        !x        = Set.elemAt i xs
        !xs'      = Set.deleteAt i xs

sampleUniformWithReplacement :: Ord a => StdGen -> Int -> [a] -> [a]
sampleUniformWithReplacement rng0 n xs0 = take n $
    go rng0
  where
    xs = Set.fromList xs0

    go !rng = x : go rng'
      where
        (i, rng') = uniformR (0, Set.size xs - 1) rng
        !x        = Set.elemAt i xs
