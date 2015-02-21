{-# LANGUAGE MultiParamTypeClasses #-}

module Crypto.Classical.Types where

import Crypto.Random (CPRG)
import Data.ByteString.Lazy (ByteString)

---

class Cipher k a where
  encrypt :: Key k => k -> ByteString -> a ByteString
  decrypt :: Key k => k -> ByteString -> a ByteString

-- | Keys can appear in a number of different forms.
-- E.g. a single number, a tuple, a mapping, etc.
-- Each needs to be interpreted uniquely by a Cipher's
-- `encrypt` and `decrypt` algorithms.
class Key a where
  -- | Randomly generate a Key.
  key :: CPRG g => g -> a
