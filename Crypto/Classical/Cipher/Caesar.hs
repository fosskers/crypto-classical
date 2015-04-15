{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module    : Crypto.Classical.Caesar
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Crypto.Classical.Cipher.Caesar where

import           Control.Lens
import           Crypto.Classical.Types
import           Crypto.Classical.Util
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Modular

---

-- | A simple Shift Cipher. The key is a number by which to shift each
-- letter in the alphabet. Example:
--
-- >>> encrypt 3 "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ^. caesar
-- "DEFGHIJKLMNOPQRSTUVWXYZABC"
newtype Caesar a = Caesar { _caesar :: a } deriving (Eq,Show,Functor)
makeLenses ''Caesar

instance Applicative Caesar where
  pure = Caesar
  Caesar f <*> Caesar a = Caesar $ f a

instance Monad Caesar where
  return = pure
  Caesar a >>= f = f a

instance Cipher (ℤ/26) Caesar where
  encrypt k = pure . B.map f
    where f c | isLower c = f $ toUpper c
              | not $ isLetter c = c
              | otherwise = toLetter $ toInt c + k

  decrypt k = encrypt (-k)
