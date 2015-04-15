{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module    : Crypto.Classical.Util
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Crypto.Classical.Util
  (
    -- * Character Conversion
    toLetter
  , toInt
    -- * Modular Arithmetic
  , inverse
    -- * Random Sequences
  , rseq
    -- * Function Inversion
  , mapInverse
  ) where

import           Control.Lens
import           Crypto.Number.Generate
import           Crypto.Number.ModArithmetic (inverseCoprimes)
import           Crypto.Random
import           Data.Char
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Modular

---

toLetter :: ℤ/26 -> Char
toLetter l = chr $ ord 'A' + (fromIntegral $ unMod l)

toInt :: Char -> ℤ/26
toInt c = toMod . toInteger $ ord c - ord 'A'

-- | Must be passed a number coprime with 26.
inverse :: ℤ/26 -> ℤ/26
inverse a = toMod $ inverseCoprimes (unMod a) 26

-- | The sequence (r1,...r[n-1]) of numbers such that r[i] is an
-- independent sample from a uniform random distribution
-- [0..n-i]
rseq :: CPRG g => g -> Integer -> [Integer]
rseq g n = rseq' g (n - 1) ^.. traverse . _1
  where rseq' :: CPRG g => g -> Integer -> [(Integer, g)]
        rseq' _ 0  = []
        rseq' g' i = (j, g') : rseq' g'' (i - 1)
          where (j, g'') = generateBetween g' 0 i

-- | Invert a Map. Keys become values, values become keys.
-- Note that this operation may result in a smaller Map than the original.
mapInverse :: Ord v => Map k v -> Map v k
mapInverse = M.foldrWithKey (\k v acc -> M.insert v k acc) M.empty
