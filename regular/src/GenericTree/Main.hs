{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module GenericTree.Main where

import qualified Data.Map                   as M
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Cata
import           Generics.Memo.Main
import           Generics.Regular.Base
import           Generics.Regular.TH

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Show)

data RoseTree a = Empty
                | NodeR a (RoseTree a) -- Recursive lists do not work with Generics
                deriving (Show)

$(deriveAll ''Tree "PFTree")
type instance PF (Tree a) = PFTree a

$(deriveAll ''RoseTree "PFRoseTree")
type instance PF (RoseTree a) = PFRoseTree a

t :: Tree Int
t = Node (Leaf 1) 2 (Leaf 3)

rt :: RoseTree Int
rt = NodeR 1 (NodeR 2 (NodeR 3 Empty))

type Merkle f = Fix (f :*: K Digest)
type MerkleTree a = Merkle (PFTree a)
type MerkleRoseTree a = Merkle (PFRoseTree a)

cataInt :: Fix (PFTree Int :*: K Digest) -> Int
cataInt = cata f
  where
    f :: (PFTree Int :*: K Digest) Int -> Int
    f (px :*: K h) = case px of
      L (C (K x))                 -> x
      R (C (I l :*: K x :*: I r)) -> l + x + r

cataHashes :: Fix (PFTree Int :*: K Digest) -> [Digest]
cataHashes = cata f
  where
    f (px :*: K h) = case px of
      L _                       -> [h]
      R (C (I l :*: _ :*: I r)) -> h : l ++ r

cataSum :: MerkleTree Int -> (Int, M.Map Digest Int)
cataSum = cataMerkle
  (\case
    L (C (K x))                 -> x
    R (C (I l :*: K x :*: I r)) -> l + x + r
  )

cataSumRose :: MerkleRoseTree Int -> (Int, M.Map Digest Int)
cataSumRose = cataMerkle f
  where
    f :: PFRoseTree Int Int -> Int
    f (L _)                 = 0
    f (R (C (K x :*: I y))) = x + y
