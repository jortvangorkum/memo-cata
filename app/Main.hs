module Main where

import           Criterion.Main
import qualified Data.Map                   as M
import qualified Data.Trie                  as T
import qualified GenericTree.GenericCata    as G
import           GenericTree.Main
import qualified GenericTree.SpecificCata   as S
import           Generics.Data.Digest.CRC32
import           Generics.Main
import           Tree

-- Generates Tree of size 2n + 1
generateTreeG :: Int -> TreeG Int
generateTreeG = from . generateTreeF
  where
    generateTreeF n = generateBinTree 0 (n - 1)
    generateBinTree :: Int -> Int -> TreeF Int
    generateBinTree l u =
      if u < l
      then In $ LeafF l
      else let i = (l + u) `div` 2
           in In $ NodeF (generateBinTree l (i - 1)) i (generateBinTree (i + 1) u)

-- BENCHMARKS
benchCataInt :: Int -> Benchmark
benchCataInt n = env (return . generateTreeG $ n) (bench (show n) . nf S.cataInt)

benchGenCataSum :: Int -> Benchmark
benchGenCataSum n = env (setupMerkleTree n) (bench (show n) . nf (G.cataSum empty))

benchSpecCataSum :: Int -> Benchmark
benchSpecCataSum  n = env (setupMerkleTree n) (bench (show n) . nf S.cataSum)

benchGenCataSumMap :: Int -> Benchmark
benchGenCataSumMap n = env (setupMapInt n) (bench (show n) . nf (uncurry G.cataSum))

benchEvaluateTrie :: Int -> Benchmark
benchEvaluateTrie n = env (setupMerkleTree n) (bench (show n) . nf generateTrie)

benchGenCataSumTrie :: Int -> Benchmark
benchGenCataSumTrie n = env (setupTrieInt n) (bench (show n) . nf (uncurry G.cataSumTrie))

benchSpecCataSumMap :: Int -> Benchmark
benchSpecCataSumMap n = env (setupMapInt n) (bench (show n) . nf (uncurry S.cataSumMap))

benchGenCataSumMapChange :: Int -> Benchmark
benchGenCataSumMapChange n = env (setupMapIntChange n) (bench (show n) . nf (uncurry G.cataSum))

benchGenCataSumTrieChange :: Int -> Benchmark
benchGenCataSumTrieChange n = env (setupTrieIntChange n) (bench (show n) . nf (uncurry G.cataSumTrie))

benchSpecCataSumMapChange :: Int -> Benchmark
benchSpecCataSumMapChange n = env (setupMapIntChange n) (bench (show n) . nf (uncurry S.cataSumMap))

-- ENVIRONMENTS
setupMerkleTree :: Int -> IO (MerkleTree Int)
setupMerkleTree = return . merkle . generateTreeG

setupMapInt :: Int -> IO (M.Map Digest Int, MerkleTree Int)
setupMapInt n = return (m, t)
  where
    m = snd . G.cataSum empty . merkle . generateTreeG $ n
    t = merkle . generateTreeG $ n

generateTrie :: MerkleTree Int -> T.Trie Int
generateTrie = snd . G.cataSumTrie empty

setupTrieInt :: Int -> IO (T.Trie Int, MerkleTree Int)
setupTrieInt n = return (m, t)
  where
    m = snd . G.cataSumTrie empty . merkle . generateTreeG $ n
    t = merkle . generateTreeG $ n

setupMapIntChange :: Int -> IO (M.Map Digest Int, MerkleTree Int)
setupMapIntChange n = return (m, t)
  where
    m = snd . G.cataSum empty . merkle . changeSingleLeaf . generateTreeG $ n
    t = merkle . generateTreeG $ n

setupTrieIntChange :: Int -> IO (T.Trie Int, MerkleTree Int)
setupTrieIntChange n = return (m, t)
  where
    m = snd . G.cataSumTrie empty . merkle . changeSingleLeaf . generateTreeG $ n
    t = merkle . generateTreeG $ n

changeSingleLeaf :: TreeG Int -> TreeG Int
changeSingleLeaf (In (Inl (K _))) = In (Inl (K 10))
changeSingleLeaf (In (Inr (Pair (Pair (I l, x), r)))) = In $ Inr $ Pair (Pair (I (changeSingleLeaf l), x), r)

main :: IO ()
main = defaultMain
  [ bgroup "Cata Sum" $
    [benchCataInt (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Generic Cata Sum" $
    [benchGenCataSum (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Specific Cata Sum" $
    [benchSpecCataSum (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Generic Cata Sum with Map" $
    [benchGenCataSumMap (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Generic Cata Sum Trie" $
    [benchEvaluateTrie (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Generic Cata Sum with Trie" $
    [benchGenCataSumTrie (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Specific Cata Sum with Map" $
    [benchSpecCataSumMap (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Generic Cata Sum with Single Change Map" $
    [benchGenCataSumMap (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Generic Cata Sum with Single Change Trie" $
    [benchGenCataSumTrieChange (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Specific Cata Sum with Single Change Map" $
    [benchSpecCataSumMap (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  ]

