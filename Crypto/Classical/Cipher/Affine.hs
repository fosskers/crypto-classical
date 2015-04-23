{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module    : Crypto.Classical.Affine
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Crypto.Classical.Cipher.Affine where

import           Control.Applicative
import           Control.Lens
import           Crypto.Classical.Types
import           Crypto.Classical.Util
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Modular

---

-- | An Affine Cipher is a non-random Substitution Cipher, such that a
-- character `x` is mapped to a cipher character according to the equation:
--
-- f(x) = ax + b (mod 26)
--
-- Also known as a Linear Cipher.
newtype Affine a = Affine { _affine :: a } deriving (Eq,Show,Functor)
makeLenses ''Affine

instance Applicative Affine where
  pure = Affine
  Affine f <*> Affine a = Affine $ f a

instance Monad Affine where
  return = pure
  Affine a >>= f = f a

instance Cipher (ℤ/26,ℤ/26) Affine where
  encrypt (a,b) = pure . B.map f
    where f c | isLower c = f $ toUpper c
              | not $ isLetter c = c
              | otherwise = letter $ a * int c + b

  decrypt (a,b) = pure . B.map f
    where f c | isLower c = f $ toUpper c
              | not $ isLetter c = c
              | otherwise = letter $ (int c - b) * inverse a
