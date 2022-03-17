{-# LANGUAGE LambdaCase #-}
module GenericTree.SpecificCata where

import           Data.ByteString            (ByteString)
import qualified Data.Map                   as M
import           Generics.Cata
import           Generics.Data.Digest.CRC32
import           Generics.Main
import           GenericTree.Main

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

cataSum :: MerkleTree Int -> (Int, M.Map ByteString Int)
cataSum = cata f
  where
    f (Pair (px, K (Digest _ h))) = case px of
      Inl (K x) -> (x, M.insert h x M.empty)
      Inr (Pair (Pair (I (xl, ml), K x), I (xr, mr)))
        -> let y = x + xl + xr
           in (y, M.insert h y (ml <> mr))

cataSumMap :: M.Map ByteString Int -> MerkleTree Int -> (Int, M.Map ByteString Int)
cataSumMap m = cata f
  where
    f (Pair (px, K (Digest _ h))) = case M.lookup h m of
      Just n -> (n, m)
      Nothing -> case px of
        Inl (K x) -> (x, M.insert h x m)
        Inr (Pair (Pair (I (xl, ml), K x), I (xr, mr)))
          -> let y = x + xl + xr
             in (y, M.insert h y (ml <> mr))
