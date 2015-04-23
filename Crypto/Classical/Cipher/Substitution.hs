{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Crypto.Classical.Substitution
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Crypto.Classical.Cipher.Substitution where

import           Control.Applicative
import           Control.Lens
import           Crypto.Classical.Types
import           Crypto.Classical.Util
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

---

-- | A Cipher whose key is a (pseudo)random mapping of characters
-- to other characters. A character may map to itself.
newtype Substitution a = Substitution { _substitution :: a }
                       deriving (Eq,Show,Functor)
makeLenses ''Substitution

instance Applicative Substitution where
  pure = Substitution
  Substitution f <*> Substitution a = Substitution $ f a

instance Monad Substitution where
  return = pure
  Substitution a >>= f = f a

instance Cipher (Map Char Char) Substitution where
  encrypt m = pure . B.map f
    where f c | isLower c = f $ toUpper c
              | otherwise = M.findWithDefault c c m

  decrypt m = encrypt (mapInverse m)
