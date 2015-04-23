{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module    : Crypto.Classical.Stream
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Crypto.Classical.Cipher.Stream where

import           Control.Applicative
import           Control.Lens
import           Crypto.Classical.Types
import           Crypto.Classical.Util
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Modular

---

-- | A Cipher with pseudorandom keys as long as the plaintext.
-- Since Haskell is lazy, our keys here are actually of infinite length.
--
-- If for whatever reason a key of finite length is given to `encrypt`,
-- the ciphertext is cutoff to match the key length. Example:
--
-- >>> encrypt [1,2,3] "ABCDEF" ^. stream
-- "BDF"
newtype Stream a = Stream { _stream :: a } deriving (Eq,Show,Functor)
makeLenses ''Stream

instance Applicative Stream where
  pure = Stream
  Stream f <*> Stream a = Stream $ f a

instance Monad Stream where
  return = pure
  Stream a >>= f = f a

instance Cipher [ℤ/26] Stream where
  encrypt k = pure . B.pack . f k . B.unpack
    where f _ [] = []
          f [] _ = []
          f (kc:ks) (m:ms) 
            | isLower m = f (kc:ks) (toUpper m : ms)
            | not $ isLetter m = m : f ks ms
            | otherwise = letter (int m + kc) : f ks ms

  decrypt k = encrypt k'
    where k' = map (* (-1)) k
