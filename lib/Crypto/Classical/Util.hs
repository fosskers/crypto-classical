{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module    : Crypto.Classical.Util
-- Copyright : (c) Colin Woodbury, 2015 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Crypto.Classical.Util
  (
    -- * Character Conversion
    letter
  , int
    -- * Modular Arithmetic
  , inverse
    -- * Random Numbers
  , prng
  , rseq
    -- * Map function
  , mapInverse
  , compose
  , (|.|)
    -- * Miscellaneous
  , uniZip
  , stretch
  , both
  ) where

import           Crypto.Number.Generate
import           Crypto.Number.ModArithmetic (inverseCoprimes)
import           Crypto.Random
import           Data.Char
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Modular

---

letter :: ℤ/26 -> Char
letter l = chr $ ord 'A' + fromIntegral (unMod l)

int :: Char -> ℤ/26
int c = toMod . toInteger $ ord c - ord 'A'

-- | Must be passed a number coprime with 26.
inverse :: ℤ/26 -> ℤ/26
inverse a = toMod $ inverseCoprimes (unMod a) 26

prng :: IO SystemRNG
prng = fmap cprgCreate createEntropyPool

-- | The sequence (r1,...r[n-1]) of numbers such that r[i] is an
-- independent sample from a uniform random distribution
-- [0..n-i]
rseq :: CPRG g => g -> Integer -> [Integer]
rseq g n = map fst $ rseq' g (n - 1)
  where
    rseq' :: CPRG g => g -> Integer -> [(Integer, g)]
    rseq' _ 0  = []
    rseq' g' i = (j, g') : rseq' g'' (i - 1)
      where (j, g'') = generateBetween g' 0 i

-- | Invert a Map. Keys become values, values become keys.
-- Note that this operation may result in a smaller Map than the original.
mapInverse :: (Ord k, Ord v) => Map k v -> Map v k
mapInverse = M.foldrWithKey (\k v acc -> M.insert v k acc) M.empty

-- | Compose two Maps. If some key `v` isn't present in the second
-- Map, then `k` will be left out of the result.
--
-- 2015 April 16 @ 13:56
-- Would it be possible to make a Category for Map like this?
compose :: (Ord k, Ord v) => Map k v -> Map v v' -> Map k v'
compose s t = M.foldrWithKey f M.empty s
  where f k v acc = case M.lookup v t of
                     Nothing -> acc
                     Just v' -> M.insert k v' acc

-- | An alias for compose. Works left-to-right.
(|.|) :: (Ord k, Ord v) => Map k v -> Map v v' -> Map k v'
(|.|) = compose

-- | Zip a list on itself. Takes pairs of values and forms a tuple.
-- Example:
--
-- >>> uniZip [1,2,3,4,5,6]
-- [(1,2),(3,4),(5,6)]
uniZip :: [a] -> [(a,a)]
uniZip []       = []
uniZip [_]      = []
uniZip (a:b:xs) = (a,b) : uniZip xs

-- | Stretch the contents of a list. List becomes twice a long.
-- List must be finite.
-- Example:
--
-- >>> stretch [1,2,3,4]
-- [1,1,2,2,3,3,4,4]
stretch :: [a] -> [a]
stretch = foldr (\x acc -> x : x : acc) []

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)
