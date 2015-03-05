{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Crypto.Classical.Types where

import           Control.Lens
import           Crypto.Classical.Shuffle
import           Crypto.Number.Generate
import           Crypto.Random (CPRG)
import           Data.ByteString.Lazy (ByteString)
import           Data.List ((\\))
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Modular

---

class Key k => Cipher k a | a -> k where
  encrypt :: k -> ByteString -> a ByteString
  decrypt :: k -> ByteString -> a ByteString

-- | Keys can appear in a number of different forms.
-- E.g. a single number, a tuple, a mapping, etc.
-- Each needs to be interpreted uniquely by a Cipher's
-- `encrypt` and `decrypt` algorithms.
class Key a where
  -- | Randomly generate a Key.
  key :: CPRG g => g -> a

instance Key (ℤ/26) where
  key g = toMod . fst . generateMax g $ 26

-- | For Affine Ciphers.
-- `a` must be coprime with 26, or else a^-1 won't exist and
-- and we can't decrypt.
instance Key (ℤ/26,ℤ/26) where
  key g = (a,b) & _1 %~ toMod
    where a = ([1,3..25] \\ [13]) !! (fromIntegral . fst . generateMax g $ 11)
          b = key g

-- | Key for Substitution Cipher. The Key is the Mapping itself.
instance Key (Map Char Char) where
  key g = M.fromList $ zip ['A'..'Z'] $ shuffle g ['A'..'Z'] 26
