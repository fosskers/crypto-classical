{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module    : Crypto.Classical.Test
-- Copyright : (c) Colin Woodbury, 2015 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Main where

import           Control.Monad (void)
import           Crypto.Classical.Cipher
import           Crypto.Classical.Letter
import           Crypto.Classical.Types
import           Crypto.Classical.Util
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import qualified Data.Foldable as F
import           Test.QuickCheck

---

-- Not to be exported, as this only generates ByteStrings
-- of capital Ascii characters.
instance Arbitrary ByteString where

  arbitrary = B.pack . map _char <$> arbitrary
instance Arbitrary Letter where
  arbitrary = Letter <$> c
    where c = do
            c' <- arbitrary
            if isAsciiUpper c'
               then return c'
               else c

---

-- | Run every test on every Cipher.
main :: IO ()
main = void . sequence $ cipherTs ++ otherTs

cipherTs :: [IO ()]
cipherTs = [ cycleT _caesar
           , cycleT _affine
           , cycleT _substitution
           , cycleT _stream
           , cycleT _vigenère
           , cycleT _enigma
           , notSelfT _caesar
           , notSelfT _affine
           , notSelfT _substitution
           , notSelfT _stream
           , notSelfT _vigenère
           , notSelfT _enigma
           , diffKeyT _caesar
           , diffKeyT _affine
           , diffKeyT _substitution
           , diffKeyT _stream
           , diffKeyT _vigenère
           , diffKeyT _enigma
           , noSelfMappingT
           ]

otherTs :: [IO ()]
otherTs = [ stretchT, plugFromT ]

-- | An encrypted message should decrypt to the original plaintext.
cycleT :: (Monad c, Cipher k c) => (c ByteString -> ByteString) -> IO ()
cycleT f = do
  k <- key <$> prng
  quickCheck (\m -> f (encrypt k m >>= decrypt k) == m)

-- | A message should never encrypt to itself.
notSelfT :: (Monad c, Cipher k c) => (c ByteString -> ByteString) -> IO ()
notSelfT f = do
  k <- key <$> prng
  quickCheck (\m -> B.length m > 1 ==> m /= e f k m)

-- | Different keys should yield different encryptions.
diffKeyT :: (Eq k,Monad c,Cipher k c) => (c ByteString -> ByteString) -> IO ()
diffKeyT f = do
  k  <- key <$> prng
  k' <- key <$> prng
  quickCheck (\m -> k /= k' && B.length m > 1 ==> e f k m /= e f k' m)

-- | A letter can never encrypt to itself.
noSelfMappingT :: IO ()
noSelfMappingT = do
  k <- key <$> prng
  quickCheck (\m -> all (uncurry (/=)) $ B.zip m (e _enigma k m))

-- | Encrypt and unwrap a message.
e :: Cipher k a => (a ByteString -> t) -> k -> ByteString -> t
e f k m = f $ encrypt k m

-- | A small manual test of Enigma.
-- enig :: IO ByteString
-- enig = do
--   k <- key <$> prng
--   pure . _enigma $ encrypt k "Das ist ein Wetterbericht. Heil Hitler."

-- | A stretch should always double the length.
stretchT :: IO ()
stretchT = quickCheck prop
  where prop :: [Int] -> Property
        prop xs = let l = length xs in l > 0 ==> length (stretch xs) == 2 * l

-- | Any list of pairs should always result in a Plugboard of 26 mappings.
plugFromT :: IO ()
plugFromT = quickCheck prop
  where
    prop :: [(Letter, Letter)] -> Bool
    prop xs = let xs' = map (both _char) xs
      in F.length (plugFrom xs') == 26
