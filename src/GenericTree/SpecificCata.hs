{-# LANGUAGE LambdaCase #-}
module GenericTree.SpecificCata where

import qualified Data.Map                   as M
import           GenericTree.Main
import           Generics.Cata
import           Generics.Data.Digest.CRC32
import           Generics.Main

cataInt :: TreeG Int -> Int
cataInt = cata (\case
  Inl (K x)                         -> x
  Inr (Pair (Pair (I l, K x), I r)) -> l + x + r)

cataMerkleInt :: TreeG Int -> Int
cataMerkleInt x = cata f mt
  where
    mt = merkle x
    f :: (:*:) (TreeGr Int) (K Digest) Int -> Int
    f (Pair (px, _)) = case px of
      Inl (K x)                         -> x
      Inr (Pair (Pair (I l, K x), I r)) -> l + x + r

cataSum :: MerkleTree Int -> (Int, M.Map Digest Int)
cataSum = cata f
  where
    f (Pair (px, K h)) = case px of
      Inl (K x) -> (x, M.insert h x M.empty)
      Inr (Pair (Pair (I (xl, ml), K x), I (xr, mr)))
        -> let y = x + xl + xr
           in (y, M.insert h y (ml <> mr))

cataSumMap :: M.Map Digest Int -> MerkleTree Int -> (Int, M.Map Digest Int)
cataSumMap m = cata f
  where
    f (Pair (px, K h)) = case M.lookup h m of
      Just n -> (n, m)
      Nothing -> case px of
        Inl (K x) -> undefined
        Inr (Pair (Pair (I (xl, ml), K x), I (xr, mr)))
          -> let y = x + xl + xr
             in (y, M.insert h y (ml <> mr))
