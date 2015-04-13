{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypto.Classical.Test where

import           Control.Monad (void)
import           Crypto.Classical.Cipher.Affine
import           Crypto.Classical.Cipher.Caesar
import           Crypto.Classical.Cipher.Stream
import           Crypto.Classical.Cipher.Substitution
import           Crypto.Classical.Cipher.Vigenere
import           Crypto.Classical.Types
import           Crypto.Random
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Test.QuickCheck

---

instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

---

gen :: IO SystemRNG
gen = fmap cprgCreate createEntropyPool

testAll :: IO ()
testAll = void $ sequence [ cycleTest _caesar
                          , cycleTest _affine
                          , cycleTest _substitution
                          , cycleTest _stream
--                          , cycleTest _vigenère
                          ]

allAscii :: ByteString -> Bool
allAscii = and . map (\c -> isAscii c && isLetter c) . B.unpack

encDec :: (Monad m, Cipher k m) => k -> ByteString -> m ByteString
encDec k m = encrypt k m >>= decrypt k

cycleTest :: (Monad c, Cipher k c) => (c ByteString -> ByteString) -> IO ()
cycleTest f = do
  k <- key <$> gen
  quickCheck $ (\m -> allAscii m ==> f (encDec k m) == B.map toUpper m)
