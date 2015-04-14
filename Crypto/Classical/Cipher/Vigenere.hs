{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Crypto.Classical.Cipher.Vigenere where

import           Control.Lens
import           Crypto.Classical.Cipher.Stream
import           Crypto.Classical.Types
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Modular

---

newtype Vigenère a = Vigenère { _vigenère :: a } deriving (Eq,Show,Functor)
makeLenses ''Vigenère

instance Applicative Vigenère where
  pure = Vigenère
  Vigenère f <*> Vigenère a = Vigenère $ f a

instance Monad Vigenère where
  return = pure
  Vigenère a >>= f = f a

-- | A Vigenère Cipher is just a Stream Cipher with a finite key,
-- shorter than the length of the plaintext.
-- Weakness here: key length is a factor of the plaintext length.
instance Cipher [ℤ/26] Vigenère where
  encrypt k m = pure . view stream . encrypt (vigKey m k) $ m
  decrypt k m = pure . view stream . decrypt (vigKey m k) $ m

vigKey :: B.ByteString -> [ℤ/26] -> [ℤ/26]
vigKey m k = concat . repeat . take (n+1) $ k
  where n = floor . logBase 2 . fromIntegral . B.length $ m
