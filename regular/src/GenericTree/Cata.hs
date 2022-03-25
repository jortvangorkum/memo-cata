{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module GenericTree.Cata where

import qualified Data.Map                   as M
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Cata
import           Generics.Memo.Main
import           Generics.Regular.Base

cataSumTree :: Tree Int -> Int
cataSumTree = sum

cataInt :: MerklePF (Tree Int) -> Int
cataInt = cata f
  where
    f :: (PFTree Int :*: K Digest) Int -> Int
    f (px :*: K h) = case px of
      L (C (K x))                 -> x
      R (C (I l :*: K x :*: I r)) -> l + x + r

cataHashes :: MerklePF (Tree Int) -> [Digest]
cataHashes = cata f
  where
    f (px :*: K h) = case px of
      L _                       -> [h]
      R (C (I l :*: _ :*: I r)) -> h : l ++ r

cataSum :: MerklePF (Tree Int) -> (Int, M.Map Digest Int)
cataSum = cataMerkle
  (\case
    L (C (K x))                 -> x
    R (C (I l :*: K x :*: I r)) -> l + x + r
  )

cataSumRose :: MerklePF (RoseTree Int) -> (Int, M.Map Digest Int)
cataSumRose = cataMerkle f
  where
    f :: PFRoseTree Int Int -> Int
    f (L _)                 = 0
    f (R (C (K x :*: I y))) = x + y
