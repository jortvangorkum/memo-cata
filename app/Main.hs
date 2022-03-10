module Main where

import           Criterion.Main
import           Data.ByteString          (ByteString)
import qualified Data.Map                 as M
import qualified Data.Trie                as T
import qualified GenericTree.GenericCata  as G
import           GenericTree.Main
import qualified GenericTree.SpecificCata as S
import           GenericTree.Zipper
import           Generics.Main

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

-- The Evaluation of lazy to strict causes that the performance results become linear instead of log,
-- because the conversion takes O(N) time
benchUpdateMerkleTree :: Int -> Benchmark
benchUpdateMerkleTree n = env (setupMerkleTree n) (bench (show n) . nf (update (const mt) [down]))
  where
    mt :: MerkleTree Int
    mt = merkle (In (Inl (K 69)))

benchIncrementalComputeMap :: Int -> Benchmark
benchIncrementalComputeMap n = env (setupMapInt n) (bench (show n) . nf (\(m, t) -> fst $ G.cataSum m (update (const mt) [down] t)))
  where
    mt :: MerkleTree Int
    mt = merkle (In (Inl (K 69)))

benchIncrementalComputeTrie :: Int -> Benchmark
benchIncrementalComputeTrie n = env (setupTrieInt n) (bench (show n) . nf (\(tr, t) -> fst $ G.cataSumTrie tr (update (const mt) [down] t)))
  where
    mt :: MerkleTree Int
    mt = merkle (In (Inl (K 69)))

-- ENVIRONMENTS
setupMerkleTree :: Int -> IO (MerkleTree Int)
setupMerkleTree = return . merkle . generateTreeG

setupMapInt :: Int -> IO (M.Map ByteString Int, MerkleTree Int)
setupMapInt n = return (m, t)
  where
    m = snd . G.cataSum empty . merkle . generateTreeG $ n
    t = merkle . generateTreeG $ n

generateTrie :: MerkleTree Int -> T.Trie Int
generateTrie = snd . G.cataSumTrie empty

setupTrieInt :: Int -> IO (T.Trie Int, MerkleTree Int)
setupTrieInt n = return (m, t)
  where
    m = snd $ G.cataSumTrie empty $ merkle $ generateTreeG n
    t = merkle . generateTreeG $ n

setupMapIntChange :: Int -> IO (M.Map ByteString Int, MerkleTree Int)
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
  [ bgroup "Cata Sum"
    [benchCataInt (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5, 6]]
  , bgroup "Incremental Update MerkleTree"
    [benchUpdateMerkleTree (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5, 6]]
  , bgroup "Incremental Compute Map"
    [benchIncrementalComputeMap (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5, 6]]
  , bgroup "Incremental Compute Trie"
    [benchIncrementalComputeTrie (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5, 6]]
  , bgroup "Generic Cata Sum"
    [benchGenCataSum (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5, 6]]
  , bgroup "Specific Cata Sum"
    [benchSpecCataSum (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Generic Cata Sum with Map"
    [benchGenCataSumMap (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Generic Cata Sum Trie" $
    [benchEvaluateTrie (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5, 6]]
  , bgroup "Generic Cata Sum with Trie"
    [benchGenCataSumTrie (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Specific Cata Sum with Map"
    [benchSpecCataSumMap (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Generic Cata Sum with Single Change Map"
    [benchGenCataSumMap (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  , bgroup "Specific Cata Sum with Single Change Map"
    [benchSpecCataSumMap (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5]]
  ]

