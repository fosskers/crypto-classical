-- |
-- Module    : Crypto.Classical.Shuffle
-- Copyright : (c) Colin Woodbury, 2015 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Code borrowed from `random-shuffle` and modified to match
-- crypto-random data types.

module Crypto.Classical.Shuffle
  (
    -- * List Scrambling
    shuffle
  ) where

import Crypto.Classical.Util
import Crypto.Random
import Data.Function (fix)

---

-- A complete binary tree, of leaves and internal nodes.
-- Internal node: Node card l r
-- where card is the number of leaves under the node.
-- Invariant: card >=2. All internal tree nodes are always full.
data Tree a = Leaf !a
            | Node !Integer !(Tree a) !(Tree a)
            deriving Show

-- Convert a sequence (e1...en) to a complete binary tree
buildTree :: [a] -> Tree a
buildTree = fix growLevel . map Leaf
  where growLevel _ [node] = node
        growLevel self l   = self $ inner l

        inner []             = []
        inner [e]            = [e]
        inner (e1 : e2 : es) = e1 `seq` e2 `seq` join e1 e2 : inner es

        join l@(Leaf _)       r@(Leaf _)       = Node 2 l r
        join l@(Node ct _ _)  r@(Leaf _)       = Node (ct + 1) l r
        join l@(Leaf _)       r@(Node ct _ _)  = Node (ct + 1) l r
        join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl + ctr) l r

-- | Given a sequence (e1,...en) to shuffle, its length, and a random
-- generator, compute the corresponding permutation of the input
-- sequence.
shuffle :: CPRG g => g -> [a] -> Integer -> [a]
shuffle g elements = shuffle' elements . rseq g

-- | Given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.
shuffle' :: [a] -> [Integer] -> [a]
shuffle' elements = shuffleTree (buildTree elements)
  where shuffleTree (Leaf e) [] = [e]
        shuffleTree tree (r : rs) =
          let (b, rest) = extractTree r tree
          in b : shuffleTree rest rs
        shuffleTree _ _ = error "[shuffle] called with lists of different lengths"

        -- Extracts the n-th element from the tree and returns
        -- that element, paired with a tree with the element
        -- deleted.
        -- The function maintains the invariant of the completeness
        -- of the tree: all internal nodes are always full.
        extractTree 0 (Node _ (Leaf e) r) = (e, r)
        extractTree 1 (Node 2 (Leaf l) (Leaf r)) = (r, Leaf l)
        extractTree n (Node c (Leaf l) r) =
          let (e, r') = extractTree (n - 1) r
          in (e, Node (c - 1) (Leaf l) r')
        extractTree n (Node n' l (Leaf e))
          | n + 1 == n' = (e, l)
        extractTree n (Node c l@(Node cl _ _) r)
          | n < cl = let (e, l') = extractTree n l
                     in (e, Node (c - 1) l' r)
          | otherwise = let (e, r') = extractTree (n - cl) r
                        in (e, Node (c - 1) l r')
        extractTree _ _ = error "[extractTree] impossible"
