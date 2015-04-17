{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module    : Crypto.Classical.Types
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Crypto.Classical.Types
  (
    -- * Cipher
    Cipher(..)
    -- * Keys
  , Key(..)
    -- * Enigma Types
  , EnigmaKey(..)
  , Rotor(..)
  , Reflector
  , Plugboard
  , name
  , turnover
  , circuit
  , rotors
  , settings
  , reflector
  , plugboard
  ) where

import           Control.Lens
import           Crypto.Classical.Shuffle
import           Crypto.Classical.Util
import           Crypto.Number.Generate
import           Crypto.Random (CPRG)
import           Data.ByteString.Lazy (ByteString)
import           Data.List ((\\))
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Modular
import           Data.Monoid ((<>))
import           Data.Text (Text)

---

-- | A Cipher must be able to encrypt and decrypt. The Cipher type
-- determines the Key type.
class Key k => Cipher k a | a -> k where
  encrypt :: k -> ByteString -> a ByteString
  decrypt :: k -> ByteString -> a ByteString

-- | Keys can appear in a number of different forms.
-- E.g. a single number, a tuple, a mapping, etc.
-- Each needs to be interpreted uniquely by a Cipher's
-- `encrypt` and `decrypt` algorithms.
class Key a where
  -- | Randomly generate a Key.
  key :: CPRG g => g -> a

instance Key (ℤ/26) where
  key g = toMod . fst $ generateBetween g 1 25

-- | For Affine Ciphers.
-- `a` must be coprime with 26, or else a^-1 won't exist and
-- and we can't decrypt.
instance Key (ℤ/26,ℤ/26) where
  key g = (a,b) & _1 %~ toMod
    where a = head $ shuffle g ([1,3..25] \\ [13]) 12
          b = key g

-- | Key for Substitution Cipher. The Key is the Mapping itself.
instance Key (Map Char Char) where
  key g = M.fromList $ zip ['A'..'Z'] $ shuffle g ['A'..'Z'] 26

-- | Key for Stream/Vigenère Cipher.
instance Key [ℤ/26] where
  key g = n : key g'
    where (n,g') = generateMax g 26 & _1 %~ toMod

---

-- | A Rotor (German: Walze) is a wheel labelled A to Z, with internal
-- wirings from each entry point to exit point. There is also a turnover
-- point, upon which a Rotor would turn its left neighbour as well.
-- Typically said turnover point is thought of in terms of letters
-- (e.g. Q->R for Rotor I). Here, we represent the turnover point as
-- a distance from A (or 0, the first entry point). As the Rotor rotates,
-- this value decrements. When it rolls back to 25 (modular arithmetic),
-- we rotate the next Rotor.
--
-- Our Rotors are letter-agnostic. That is, they only map numeric
-- entry points to exit points. This allows us to simulate rotation
-- very simply with Lenses.
data Rotor = Rotor { _name     :: Text
                   , _turnover :: ℤ/26
                   , _circuit  :: Map (ℤ/26) (ℤ/26) } deriving (Eq,Show)
makeLenses ''Rotor

-- | Rotor I: Turnover from Q to R.
rI :: Rotor
rI = Rotor "I" (int 'Q') $ M.fromList (pairs & traverse . both %~ int)
  where pairs = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "EKMFLGDQVZNTOWYHXUSPAIBRCJ"

-- | Rotor II: Turnover from E to F.
rII :: Rotor
rII = Rotor "II" (int 'E') $ M.fromList (pairs & traverse . both %~ int)
  where pairs = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "AJDKSIRUXBLHWTMCQGZNPYFVOE"

-- | Rotor III: Turnover from V to W.
rIII :: Rotor
rIII = Rotor "III" (int 'V') $ M.fromList (pairs & traverse . both %~ int)
  where pairs = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "BDFHJLCPRTXVZNYEIWGAKMUSQO"

-- | Rotor IV: Turnover from J to K.
rIV :: Rotor
rIV = Rotor "IV" (int 'J') $ M.fromList (pairs & traverse . both %~ int)
  where pairs = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "ESOVPZJAYQUIRHXLNFTGKDCMWB"

-- | Rotor V: Turnover from Z to A.
rV :: Rotor
rV = Rotor "V" (int 'Z') $ M.fromList (pairs & traverse . both %~ int)
  where pairs = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "VZBRGITYUPSDNHLXAWMJQOFECK"

-- | A unmoving map, similar to the Rotors, which feeds the electrical
-- current back into Rotors. This would never feed the left Rotor's letter
-- back to itself, meaning a plaintext character would never encrypt
-- to itself. This was a major weakness in scheme which allowed the Allies
-- to make Known Plaintext Attacks against the machine.
type Reflector = Map (ℤ/26) (ℤ/26)

ukwB :: Reflector
ukwB = M.fromList (pairs & traverse . both %~ int)
  where pairs = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "YRUHQSLDPXNGOKMIEBFZCWVJAT"

-- | A set of 10 pairs of connected letters which would map letters
-- to other ones both before and after being put through the Rotors.
-- The remaining six unpaired letters can be thought of mapping to themselves.
type Plugboard = Map (ℤ/26) (ℤ/26)

-- | Essentially the machine itself. It is made up of:
-- 1. Three rotor choices from five, in a random placement.
-- 2. Initial settings of those Rotors.
-- 3. The Reflector model in use.
-- 4. Plugboard settings (pairs of characters).
data EnigmaKey = EnigmaKey { _rotors    :: [Rotor]
                           , _settings  :: [Char]
                           , _reflector :: Reflector
                           , _plugboard :: Plugboard
                           } deriving (Eq,Show)
makeLenses ''EnigmaKey

-- | Note that the randomly generated initial Rotor positions are not
-- applied to the Rotors when the key is generated. They have to
-- be applied before first use.
instance Key EnigmaKey where
  key g = EnigmaKey rs ss ukwB $ genPlugs g
    where rn = 3  -- Number of Rotors to use.
          rs = take rn $ shuffle g [rI,rII,rIII,rIV,rV] 5
          ss = randChars g rn

randChars :: CPRG g => g -> Int -> [Char]
randChars _ 0 = []
randChars g n = c : randChars g' (n-1)
  where (c,g') = generateBetween g 0 25 & _1 %~ letter . toMod

-- | Generate settings for the Plugboard. Ten pairs of characters will
-- be mapped to each other, and the remaining six characters will map
-- to themselves.
genPlugs :: CPRG g => g -> Plugboard
genPlugs g = M.fromList (pairs <> singles)
  where shuffled = shuffle g [0..25] 26
        (ps,ss)  = (take 20 shuffled, drop 20 shuffled)
        pairs    = foldr (\(k,v) acc -> (k,v) : (v,k) : acc) [] $ uniZip ps
        singles  = foldr (\v acc -> (v,v) : acc) [] ss
