{-# LANGUAGE LambdaCase #-}
module Helper.Main where

import           GenericTree.Main
import           Generics.Memo.Main
import           Generics.Regular.Base

merkleTreeSize :: MerkleTree a -> Int
merkleTreeSize (In (x :*: _)) = case x of
  L _                       -> 1
  R (C (I l :*: _ :*: I r)) -> 1 + lh + rh
    where
      lh = merkleTreeSize l
      rh = merkleTreeSize r

treeSize :: Tree a -> Int
treeSize = \case
  Leaf _     -> 1
  Node l _ r -> 1 + lh + rh
    where
      lh = treeSize l
      rh = treeSize r
