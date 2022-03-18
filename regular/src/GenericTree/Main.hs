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

-- | Generates Tree of size 2n + 1
generateTree :: Int -> Tree Int
generateTree = generateTreeF
  where
    generateTreeF n = generateBinTree 0 (n - 1)
    generateBinTree :: Int -> Int -> Tree Int
    generateBinTree l u =
      if u < l
      then Leaf l
      else let i = (l + u) `div` 2
           in Node (generateBinTree l (i - 1)) i (generateBinTree (i + 1) u)
