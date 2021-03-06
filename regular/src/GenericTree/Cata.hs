{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

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
cataInt = cata f
  where
    f :: (PFTree Int :*: K Digest) Int -> Int
    f (px :*: _) = case px of
      L (C (K x))                 -> x
      R (C (I l :*: K x :*: I r)) -> l + x + r

cataHashes :: MerklePF (Tree Int) -> [Digest]
cataHashes = cata f
  where
    f (px :*: K h) = case px of
      L _                       -> [h]
      R (C (I l :*: _ :*: I r)) -> h : l ++ r

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
