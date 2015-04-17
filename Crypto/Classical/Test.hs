{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Crypto.Classical.Test
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Crypto.Classical.Test
  (
    -- * Single Tests
    cycleT
  , notSelfT
    -- * Batch Tests
  , testAll
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Crypto.Classical.Cipher.Affine
import           Crypto.Classical.Cipher.Caesar
import           Crypto.Classical.Cipher.Stream
import           Crypto.Classical.Cipher.Substitution
import           Crypto.Classical.Cipher.Vigenere
import           Crypto.Classical.Letter
import           Crypto.Classical.Types
import           Crypto.Random
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Test.QuickCheck

---

-- Not to be exported, as this only generates ByteStrings
-- of capital Ascii characters.
instance Arbitrary ByteString where
  arbitrary = B.pack . map _char <$> arbitrary

---

gen :: IO SystemRNG
gen = fmap cprgCreate createEntropyPool

-- | Run every test on every Cipher.
testAll :: IO ()
testAll = void $ sequence [ cycleT $ view caesar
                          , cycleT $ view affine
                          , cycleT $ view substitution
                          , cycleT $ view stream
                          , cycleT $ view vigenère
                          , notSelfT $ view caesar
                          , notSelfT $ view affine
                          , notSelfT $ view substitution
                          , notSelfT $ view stream
                          , notSelfT $ view vigenère
                          ]

-- | An encrypted message should decrypt to the original plaintext.
cycleT :: (Monad c, Cipher k c) => (c ByteString -> ByteString) -> IO ()
cycleT f = do
  k <- key <$> gen
  quickCheck (\m -> f (encrypt k m >>= decrypt k) == m)

-- | A message should never encrypt to itself.
notSelfT :: (Monad c, Cipher k c) => (c ByteString -> ByteString) -> IO ()
notSelfT f = do
  k <- key <$> gen
  quickCheck (\m -> B.length m > 1 ==> m /= f (encrypt k m))

{- 2015 April 16 @ 15:32 - This won't compile.
uniZipT :: IO ()
uniZipT = quickCheck (\l -> length (uniZip l) == (length l `div` 2))
-}
