-- |
-- Module    : Crypto.Classical.Letter
-- Copyright : (c) Colin Woodbury, 2015 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Crypto.Classical.Letter where

---

-- | A `Letter` is a capital Ascii letter (A-Z)
newtype Letter = Letter { _char :: Char } deriving (Eq,Show)
