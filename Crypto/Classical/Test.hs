{-# LANGUAGE OverloadedStrings #-}

module Crypto.Classical.Test where

import Control.Applicative ((<$>))
import Crypto.Classical.Cipher.Affine
import Crypto.Classical.Cipher.Caesar
import Crypto.Classical.Cipher.Stream
import Crypto.Classical.Cipher.Substitution
import Crypto.Classical.Types
import Crypto.Random
import Data.ByteString.Lazy (ByteString)

---

foo :: IO SystemRNG
foo = fmap cprgCreate createEntropyPool

testIO :: Cipher k c => ByteString -> IO (c ByteString)
testIO s = do
  k <- fmap key foo
  return $ encrypt k s

testAll :: ByteString -> IO [ByteString]
testAll s = sequence [ caesar       <$> testIO s
                     , affine       <$> testIO s
                     , substitution <$> testIO s
                     , stream       <$> testIO s
                     ]

plain :: ByteString
plain = "I QUITE ENJOY THIS! DON'T YOU?"

alpha :: ByteString
alpha = "abcdefghijklmnopqrstuvwxyz"
