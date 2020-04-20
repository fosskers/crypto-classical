{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- |
-- Module    : Crypto.Classical.Vigenere
-- Copyright : (c) Colin Woodbury, 2015 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Crypto.Classical.Cipher.Vigenere where

import           Crypto.Classical.Cipher.Stream
import           Crypto.Classical.Types
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Modular

---

-- | A Vigenère Cipher is just a Stream Cipher with a finite key,
-- shorter than the length of the plaintext. The key is repeated for
-- the entire length of the plaintext.
newtype Vigenère a = Vigenère { _vigenère :: a } deriving (Eq,Show,Functor)

instance Applicative Vigenère where
  pure = Vigenère
  Vigenère f <*> Vigenère a = Vigenère $ f a

instance Monad Vigenère where
  return = pure
  Vigenère a >>= f = f a

instance Cipher [ℤ/26] Vigenère where
  encrypt k m = pure . _stream . encrypt (vigKey m k) $ m
  decrypt k m = pure . _stream . decrypt (vigKey m k) $ m

-- | Determine a Vigenère key from a Stream key.
-- Weakness here: key length is a factor of the plaintext length.
vigKey :: B.ByteString -> [ℤ/26] -> [ℤ/26]
vigKey m = concat . repeat . take (n+1)
  where n = floor @Double . logBase 2 . fromIntegral . B.length $ m
