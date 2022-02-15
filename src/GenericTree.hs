{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
module GenericTree where

import qualified Data.Map                   as M
import           Debug.Trace                (trace)
import           Generics
import           Generics.Cata
import           Generics.Data.Digest.CRC32
import           Tree


type MerkleFix f = Fix (f :*: K Digest)
type MerkleTree a = MerkleFix (TreeGr a)

type TreeG  a = Fix (TreeGr a)

type TreeGr a = K a
             :+: ((I :*: K a) :*: I)


cataSum2 :: TreeG Int -> (Int, M.Map Digest Int)
cataSum2 = cataMerkleMap cataSum M.empty . merkle
  where
    cataSum = \case
      Inl (K x)                         -> x
      Inr (Pair (Pair (I l, K x), I r)) -> l + x + r

cataSum2WithMap :: M.Map Digest Int -> TreeG Int -> (Int, M.Map Digest Int)
cataSum2WithMap m x = cataMerkleMap cataSum m t
  where
    t = merkle x
    cataSum = \case
      Inl (K x)                         -> x
      Inr (Pair (Pair (I l, K x), I r)) -> l + x + r

from :: TreeF a -> TreeG a
from = cata f
  where
    f :: TreeFr a (TreeG a) -> TreeG a
    f (LeafF x)     = In $ Inl $ K x
    f (NodeF l x r) = In $ Inr $ Pair (Pair (I l, K x), I r)

exampleTreeG :: TreeG Int
exampleTreeG = from exampleTreeF

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

cataMerkleTree :: (Show a) => (a -> Digest -> b) -> (b -> a -> b -> Digest -> b) -> MerkleTree a -> b
cataMerkleTree leaf node = cata f
  where
    f (Pair (px, K h)) = case px of
      Inl (K x)                         -> leaf x h
      Inr (Pair (Pair (I l, K x), I r)) -> node l x r h

cataTreeG :: (Show a) => (a -> Digest -> b) -> (b -> a -> b -> Digest -> b) -> TreeG a -> b
cataTreeG leaf node x = let mt = merkle x in cataMerkleTree leaf node mt

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

cataMerkleMapFib :: MerkleTree Int -> (Int, M.Map String Int)
cataMerkleMapFib = cataMerkleTree leaf node
  where
    leaf x h = let n = fib x
               in (n, M.insert (debugHash h) n M.empty)
    node (xl, ml) x (xr, mr) h = let n = fib x + xl + xr
                                 in (n, M.insert (debugHash h) n (ml <> mr))

cataMerkleMapFibWithMap :: M.Map String Int -> MerkleTree Int -> (Int, M.Map String Int)
cataMerkleMapFibWithMap m = cataMerkleTree leaf node
  where
    leaf x h = case M.lookup (debugHash h) m of
      Nothing -> let n = fib x in (n, M.insert (debugHash h) n M.empty)
      Just n  -> (n, m)
    node (xl, ml) x (xr, mr) h = case M.lookup (debugHash h) m of
      Nothing -> let n = fib x + xl + xr in (n, M.insert (debugHash h) n (ml <> mr))
      Just n -> (n, m)

cataMerkleToMap :: MerkleTree Int -> (Int, M.Map String Int)
cataMerkleToMap = cataMerkleTree leaf node
  where
    leaf x h = (x, M.insert (debugHash h) x M.empty)
    node (xl, ml) x (xr, mr) h = let x' = x + xl + xr
                                 in (x', M.insert (debugHash h) x' (ml <> mr))

cataMap :: TreeG Int -> (Int, M.Map String Int)
cataMap = cataTreeG leaf node
  where
    leaf x h = (x, M.insert (debugHash h) x M.empty)
    node (xl, ml) x (xr, mr) h = let x' = x + xl + xr
                                 in (x', M.insert (debugHash h) x' (ml <> mr))

cataWithMap :: M.Map String Int -> TreeG Int -> (Int, M.Map String Int)
cataWithMap m = cataTreeG leaf node
  where
    leaf x h = (x, M.insert (debugHash h) x M.empty)
    node (xl, ml) x (xr, mr) h = case M.lookup (debugHash h) m of
      Nothing -> let x' = x + xl + xr
                 in (x', M.insert (debugHash h) x' (ml <> mr))
      Just n  -> (n, m)

cataMerkleWithMapStop :: M.Map String Int -> MerkleTree Int -> (Int, M.Map String Int)
cataMerkleWithMapStop m (In (Pair (x, K h))) = case M.lookup (debugHash h) m of
  Nothing -> y
  Just n  -> (n, m)
  where
    y = case x of
      Inl (K x) -> (x, M.insert (debugHash h) x M.empty)
      Inr (Pair (Pair (I l, K x), I r)) -> (x', m')
        where
          (xl, ml) = cataMerkleWithMapStop m l
          (xr, mr) = cataMerkleWithMapStop ml r
          x' = x + xl + xr
          m' = M.insert (debugHash h) x' mr

cataMerkleWithMap :: M.Map String Int -> MerkleTree Int -> (Int, M.Map String Int)
cataMerkleWithMap m = cataMerkleTree leaf node
  where
    leaf x h = (x, M.insert (debugHash h) x M.empty)
    node (xl, ml) x (xr, mr) h = case M.lookup (debugHash h) m of
      Nothing -> let x' = x + xl + xr
                 in (x', M.insert (debugHash h) x' (ml <> mr))
      Just n  -> (n, m)

showTreeG :: String
showTreeG = show $ merkleG $ unFix exampleTreeG

getRootDigest :: MerkleTree a -> Digest
getRootDigest (In (Pair (_, K h))) = h
