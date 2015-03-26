{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Crypto.Classical.Cipher.Stream where

import           Control.Applicative
import           Control.Lens
import           Crypto.Classical.Types
import           Crypto.Classical.Util
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Modular

---

data Stream a = Stream { _stream :: a } deriving (Eq,Show,Functor)
makeLenses ''Stream

instance Applicative Stream where
  pure = Stream
  Stream f <*> Stream a = Stream $ f a

instance Monad Stream where
  return = pure
  Stream a >>= f = f a

instance Cipher [ℤ/26] Stream where
  encrypt k = pure . B.pack . f k . B.unpack
    where f _ [] = []
          f (kc:ks) (m:ms) 
            | isLower m = f (kc:ks) (toUpper m : ms)
            | not $ isLetter m = m : f ks ms
            | otherwise = toLetter (toInt m + kc) : f ks ms

  decrypt k = encrypt k'
    where k' = map (* (-1)) k
