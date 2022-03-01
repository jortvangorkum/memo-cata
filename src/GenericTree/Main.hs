{-# LANGUAGE TypeOperators #-}
module GenericTree.Main where

import           Generics.Cata
import           Generics.Data.Digest.CRC32
import           Generics.Main
import           Tree

type MerkleFix f = Fix (f :*: K Digest)
type MerkleTree a = MerkleFix (TreeGr a)

type TreeG  a = Fix (TreeGr a)

type TreeGr a = K a
             :+: ((I :*: K a) :*: I)

from :: TreeF a -> TreeG a
from = cata f
  where
    f :: TreeFr a (TreeG a) -> TreeG a
    f (LeafF x)     = In $ Inl $ K x
    f (NodeF l x r) = In $ Inr $ Pair (Pair (I l, K x), I r)

exampleTreeG :: TreeG Int
exampleTreeG = from exampleTreeF

showTreeG :: String
showTreeG = show exampleTreeG

getRootDigest :: MerkleTree a -> Digest
getRootDigest (In (Pair (_, K h))) = h

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Generates Tree of size 2n + 1
generateTreeG :: Int -> TreeG Int
generateTreeG = from . generateTreeF
  where
    generateTreeF n = generateBinTree 0 (n - 1)
    generateBinTree :: Int -> Int -> TreeF Int
    generateBinTree l u =
      if u < l
      then In $ LeafF l
      else let i = (l + u) `div` 2
           in In $ NodeF (generateBinTree l (i - 1)) i (generateBinTree (i + 1) u)
