{-# LANGUAGE LambdaCase #-}
module GenericTree.GenericCata where

import           Data.ByteString            (ByteString)
import qualified Data.Map                   as M
import qualified Data.Trie                  as T
import           GenericTree.Main
import           Generics.Cata
import           Generics.Data.Digest.CRC32
import           Generics.Main

cataSum :: M.Map ByteString Int -> MerkleTree Int -> (Int, M.Map ByteString Int)
cataSum = cataMerkleMap
  (\case
      Inl (K x)                         -> x
      Inr (Pair (Pair (I l, K x), I r)) -> l + x + r
  )

cataSumTrie :: T.Trie Int -> MerkleTree Int -> (Int, T.Trie Int)
cataSumTrie = cataMerkleMap
  (\case
      Inl (K x)                         -> x
      Inr (Pair (Pair (I l, K x), I r)) -> l + x + r
  )

cataFib :: M.Map ByteString Int -> MerkleTree Int -> (Int, M.Map ByteString Int)
cataFib = cataMerkleMap
  (\case
      Inl _ -> fib 50
      Inr _ -> fib 50
  )
