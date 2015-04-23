{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module    : Crypto.Classical.Vigenere
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Crypto.Classical.Cipher.Vigenere where

import           Control.Applicative
import           Control.Lens
import           Crypto.Classical.Cipher.Stream
import           Crypto.Classical.Types
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Modular

---

-- | A Vigenère Cipher is just a Stream Cipher with a finite key,
-- shorter than the length of the plaintext. The key is repeated for
-- the entire length of the plaintext.
newtype Vigenère a = Vigenère { _vigenère :: a } deriving (Eq,Show,Functor)
makeLenses ''Vigenère

instance Applicative Vigenère where
  pure = Vigenère
  Vigenère f <*> Vigenère a = Vigenère $ f a

instance Monad Vigenère where
  return = pure
  Vigenère a >>= f = f a

instance Cipher [ℤ/26] Vigenère where
  encrypt k m = pure . view stream . encrypt (vigKey m k) $ m
  decrypt k m = pure . view stream . decrypt (vigKey m k) $ m

-- | Determine a Vigenère key from a Stream key.
-- Weakness here: key length is a factor of the plaintext length.
vigKey :: B.ByteString -> [ℤ/26] -> [ℤ/26]
vigKey m k = concat . repeat . take (n+1) $ k
  where n = floor . logBase 2 . fromIntegral . B.length $ m
