{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypto.Classical.Test ( testAll, cycleTest ) where

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

testAll :: IO ()
testAll = void $ sequence [ cycleTest $ view caesar
                          , cycleTest $ view affine
                          , cycleTest $ view substitution
                          , cycleTest $ view stream
                          , cycleTest $ view vigenère
                          ]

{-| An encrypted message should decrypt to the original plaintext -}
cycleTest :: (Monad c, Cipher k c) => (c ByteString -> ByteString) -> IO ()
cycleTest f = do
  k <- key <$> gen
  quickCheck $ (\m -> f (encrypt k m >>= decrypt k) == m)
