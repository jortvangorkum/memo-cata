{-# LANGUAGE LambdaCase #-}
module GenericTree.GenericCata where

import qualified Data.Map                   as M
import           GenericTree.Main
import           Generics.Cata
import           Generics.Data.Digest.CRC32
import           Generics.Main

cataSum :: M.Map Digest Int -> MerkleTree Int -> (Int, M.Map Digest Int)
cataSum = cataMerkleMap
  (\case
      Inl (K x)                         -> x
      Inr (Pair (Pair (I l, K x), I r)) -> l + x + r
  )

cataFib :: M.Map Digest Int -> MerkleTree Int -> (Int, M.Map Digest Int)
cataFib = cataMerkleMap
  (\case
      Inl _ -> fib 50
      Inr _ -> fib 50
  )
