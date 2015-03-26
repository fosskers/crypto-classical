{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypto.Classical.Test where

import Control.Applicative ((<$>))
import Crypto.Classical.Cipher.Affine
import Crypto.Classical.Cipher.Caesar
import Crypto.Classical.Cipher.Stream
import Crypto.Classical.Cipher.Substitution
import Crypto.Classical.Cipher.Vigenere
import Crypto.Classical.Types
import Crypto.Random
import Data.ByteString.Lazy (ByteString)

---

gen :: IO SystemRNG
gen = fmap cprgCreate createEntropyPool

testIO :: (Functor c, Monad c) => Cipher k c => ByteString -> IO (c (ByteString,ByteString))
testIO s = do
  k <- fmap key gen
  return $ encrypt k s >>= \c -> (fmap (c,) $ decrypt k c)

testAll :: ByteString -> IO [(ByteString,ByteString)]
testAll s = sequence [ _caesar       <$> testIO s
                     , _affine       <$> testIO s
                     , _substitution <$> testIO s
                     , _stream       <$> testIO s
                     , _vigenère     <$> testIO s
                     ]

plain :: ByteString
plain = "I QUITE ENJOY THIS! DON'T YOU?"

alpha :: ByteString
alpha = "abcdefghijklmnopqrstuvwxyz"
