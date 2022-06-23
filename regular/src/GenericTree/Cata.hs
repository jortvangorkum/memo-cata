{-# LANGUAGE LambdaCase #-}

module GenericTree.Cata where

import           Data.ByteString         (ByteString)
import qualified Data.HashMap.Strict     as H
import qualified Data.Map                as M
import           GenericTree.Main
import           Generics.Data.Digest
import           Generics.Memo.Cata.Main
import           Generics.Memo.Main
import           Generics.Regular.Base

cataSumTree :: Tree Int -> Int
cataSumTree = sum

cataInt :: MerklePF (Tree Int) -> Int
cataInt = cata
  (\case
    L (C (K x))                 -> x
    R (C (I l :*: K x :*: I r)) -> l + x + r
  )

cataSum :: MerklePF (Tree Int) -> (Int, H.HashMap Digest Int)
cataSum = cataMerkle
  (\case
    L (C (K x))                 -> x
    R (C (I l :*: K x :*: I r)) -> l + x + r
  )

cataSumMap :: H.HashMap Digest Int -> MerklePF (Tree Int) -> (Int, H.HashMap Digest Int)
cataSumMap = cataMerkleMap
  (\case
    L (C (K x))                 -> x
    R (C (I l :*: K x :*: I r)) -> l + x + r
  )

cataHeight :: MerklePF (Tree Int) -> (Int, H.HashMap Digest Int)
cataHeight = cataMerkle
  (\case
    L (C (K x))                 -> 1
    R (C (I l :*: K x :*: I r)) -> 1 + max l r
  )
