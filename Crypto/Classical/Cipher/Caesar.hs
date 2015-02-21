{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module Crypto.Classical.Cipher.Caesar where

import           Control.Applicative
import           Crypto.Classical.Types
import           Crypto.Number.Generate
import           Crypto.Random
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char

---

-- | An Integer Ring.
newtype Z26 = Z26 { z26 :: Integer } deriving (Eq,Show)

instance Key Z26 where
  key g = Z26 . fst . generateMax g $ 26

---  

data Caesar a = Caesar a deriving (Eq,Show,Functor)

instance Applicative Caesar where
  pure = Caesar
  Caesar f <*> Caesar a = Caesar $ f a

instance Monad Caesar where
  return = pure
  Caesar a >>= f = f a

instance Cipher Z26 Caesar where
  encrypt (Z26 k) = Caesar . B.map f
    where f ' ' = ' '
          f c | isLower c = f $ toUpper c
              | otherwise = toLetter ((toInt c + k') `mod` 26)
          toLetter l = chr $ ord 'a' + l
          toInt    c = ord c - ord 'a'
          k' = fromIntegral k

  decrypt (Z26 k) = encrypt (Z26 (-k))

foo :: IO SystemRNG
foo = fmap cprgCreate createEntropyPool

test :: IO (Caesar B.ByteString)
test = do
  k <- fmap key foo :: IO Z26  -- I don't want to have to state this.
  return $ encrypt k "Encrypt me" >>= decrypt k
