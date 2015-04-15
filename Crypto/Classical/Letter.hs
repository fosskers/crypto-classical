module Crypto.Classical.Letter where

-- |
-- Module    : Crypto.Classical.Letter
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

import Data.Char
import Test.QuickCheck

---

-- | A `Letter` is a capital Ascii letter (A-Z)
data Letter = Letter { _char :: Char } deriving (Eq,Show)

instance Arbitrary Letter where
  arbitrary = Letter <$> c
    where c = do
            c' <- arbitrary
            if isAsciiUpper c'
               then return c'
               else c
