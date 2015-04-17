{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module    : Crypto.Classical.Cipher.Enigma
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Crypto.Classical.Cipher.Enigma where

import           Control.Lens
import           Control.Monad.Trans.State.Lazy
import           Crypto.Classical.Types
import           Crypto.Classical.Util
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Maybe (fromJust)
import           Data.Modular

---

newtype Enigma a = Enigma { _enigma :: a } deriving (Eq,Show,Functor)
makeLenses ''Enigma

instance Applicative Enigma where
  pure = Enigma
  Enigma f <*> Enigma a = Enigma $ f a

instance Monad Enigma where
  return = pure
  Enigma a >>= f = f a

-- | Decryption is the same as encryption.
instance Cipher EnigmaKey Enigma where
  decrypt = encrypt
  encrypt k m = pure . B.pack $ evalState (traverse f $ B.unpack m) k'
    where k' = withInitialSettings k
          f c | not $ isLetter c = return c
              | isLower c = f $ toUpper c
              | otherwise = do
                  modify (& rotors %~ turn)
                  (EnigmaKey rots _ rl pl) <- get
                  let rs  = map _circuit rots
                      rs' = reverse $ map mapInverse rs
                      pl' = mapInverse pl
                      cmp = foldl1 compose
                      e   = pl |.| cmp rs |.| rl |.| cmp rs' |.| pl'
                  return . letter . fromJust . flip M.lookup e $ int c

-- | Applies the initial Rotor settings as defined in the Key to
-- the Rotors themselves. These initial rotations do not trigger
-- the turnover of neighbouring Rotors as usual.
withInitialSettings :: EnigmaKey -> EnigmaKey
withInitialSettings k = k & rotors .~ zipWith f (k ^. rotors) (k ^. settings)
  where f r s = (r & circuit %~ rotate (int s)
                   & turnover -~ (int s))

-- | Turn the (machine's) right-most (left-most in List) Rotor by one
-- position. If its turnover value wraps back to 25, then turn the next
-- Rotor as well.
turn :: [Rotor] -> [Rotor]
turn []     = []
turn (r:rs) = if (r' ^. turnover) == 25 then r' : turn rs else r' : rs
  where r' = r & circuit %~ rotate 1 & turnover -~ 1

-- | Rotate a Rotor by `n` positions. By subtracting 1 from every key
-- and value, we perfectly simulate rotation. Example:
--
-- >>> rotate $ M.fromList [(0,2),(1,0),(2,3),(3,4),(4,1)]
-- M.fromList [(4,1),(0,4),(1,2),(2,3),(3,0)]
rotate :: ℤ/26 -> Map (ℤ/26) (ℤ/26) -> Map (ℤ/26) (ℤ/26)
rotate n r = M.fromList (itoList r & traverse . both -~ n)