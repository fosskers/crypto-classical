{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Crypto.Classical.Util where

import Crypto.Number.ModArithmetic (inverseCoprimes)
import Crypto.Random
import Data.Char
import Data.Modular

---

toLetter :: ℤ/26 -> Char
toLetter l = chr $ ord 'A' + (fromIntegral $ unMod l)

toInt :: Char -> ℤ/26
toInt c = toMod . toInteger $ ord c - ord 'A'

-- | Must be passed a number coprime with 26.
inverse :: ℤ/26 -> ℤ/26
inverse a = toMod $ inverseCoprimes (unMod a) 26

shuffle :: CPRG g => g -> [a] -> [a]
shuffle = undefined
