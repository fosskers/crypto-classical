module Crypto.Classical.Letter where

import Data.Char
import Test.QuickCheck

---

{-| A `Letter` is a capital Ascii letter (A-Z) -}
data Letter = Letter { _char :: Char } deriving (Eq,Show)

instance Arbitrary Letter where
  arbitrary = Letter <$> c
    where c = do
            c' <- arbitrary
            if isAsciiUpper c'
               then return c'
               else c
