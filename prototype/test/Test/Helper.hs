module Test.Helper where

import           Generics.Data.Digest.CRC32
import           Generics.Main
import           GenericTree.Main

getRootHash :: MerkleTree Int -> Digest
getRootHash (In (Pair (_, K h))) = h

merkleTreeSize :: MerkleTree a -> Int
merkleTreeSize (In (Pair (x, _))) = case x of
  Inl k -> 1
  Inr (Pair (Pair (I l, _), I r)) -> 1 + lx + rx
    where
      lx = merkleTreeSize l
      rx = merkleTreeSize r

treeSize :: TreeG a -> Int
treeSize (In x) = case x of
  Inl k -> 1
  Inr (Pair (Pair (I l, _), I r)) -> 1 + lx + rx
    where
      lx = treeSize l
      rx = treeSize r
