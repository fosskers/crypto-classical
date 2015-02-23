{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Crypto.Classical.Cipher.Affine where

import           Control.Applicative
import           Crypto.Classical.Types
import           Crypto.Classical.Util
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Modular

---

data Affine a = Affine { affine :: a } deriving (Eq,Show,Functor)

instance Applicative Affine where
  pure = Affine
  Affine f <*> Affine a = Affine $ f a

instance Monad Affine where
  return = pure
  Affine a >>= f = f a

instance Cipher (ℤ/26,ℤ/26) Affine where
  encrypt (a,b) = Affine . B.map f
    where f c | isLower c = f $ toUpper c
              | not $ isLetter c = c
              | otherwise = toLetter $ a * toInt c + b

  decrypt (a,b) = Affine . B.map f
    where f c | isLower c = f $ toUpper c
              | not $ isLetter c = c
              | otherwise = toLetter $ (toInt c - b) * inverse a
