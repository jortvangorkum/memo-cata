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

data Tree = Leaf Int
          | Node Tree Int Tree
          deriving (Show)

$(deriveAll ''Tree "PFTree")
type instance PF Tree = PFTree

t :: Tree
t = Node (Leaf 1) 2 (Leaf 3)

type MerkleTree = Fix (PFTree :*: K Digest)

cataInt :: Fix (PFTree :*: K Digest) -> Int
cataInt = cata f
  where
    f :: (PFTree :*: K Digest) Int -> Int
    f (px :*: K h) = case px of
      L (C (K x))                 -> x
      R (C (I l :*: K x :*: I r)) -> l + x + r

cataHashes :: Fix (PFTree :*: K Digest) -> [Digest]
cataHashes = cata f
  where
    f (px :*: K h) = case px of
      L _                       -> [h]
      R (C (I l :*: _ :*: I r)) -> h : l ++ r

cataSum :: MerkleTree -> (Int, M.Map Digest Int)
cataSum = cataMerkle
  (\case
    L (C (K x))                 -> x
    R (C (I l :*: K x :*: I r)) -> l + x + r
  )
