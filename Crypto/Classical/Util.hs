{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Crypto.Classical.Util where

import Data.Char
import Data.Modular

---

toLetter :: ℤ/26 -> Char
toLetter l = chr $ ord 'A' + (fromIntegral $ unMod l)

toInt :: Char -> ℤ/26
toInt c = toMod . toInteger $ ord c - ord 'A'

-- Borrowed from Wikipedia. Kinda terrible.
inverse :: ℤ/26 -> ℤ/26
inverse a = toMod $ work 0 1 26 (unMod a)
  where work t _ _ 0 | t < 0 = t + 26
                     | otherwise = t
        work t newt r newr = let q = r `div` newr
                             in work newt (t - q * newt) newr (r - q * newr)
