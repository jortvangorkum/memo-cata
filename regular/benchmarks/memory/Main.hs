module Main where

import           Criterion.Main
import qualified Data.Map                   as M
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Memo.Zipper

-- ENVIRONMENTS
setupMerkleTree :: Int -> IO (MerklePF (Tree Int))
setupMerkleTree = return . merkle . generateTree

setupMapInt :: Int -> IO (M.Map Digest Int, MerklePF (Tree Int))
setupMapInt n = do t <- setupMerkleTree n
                   let m = snd $ cataSum t
                   return (m, t)

-- BENCHMARKS
benchCataInt :: Int -> Benchmark
benchCataInt n = env (return . generateTree $ n) (bench (show n) . nf cataSumTree)

benchGenCataSum :: Int -> Benchmark
benchGenCataSum n = env (setupMerkleTree n) (bench (show n) . nf cataSum)

benchIncrementalComputeMap :: Int -> Benchmark
benchIncrementalComputeMap n = env (setupMapInt n) (bench (show n) . nf (\(m, t) -> fst $ cataSumMap m (update (const mt) [Bttm] t)))
  where
    mt :: MerklePF (Tree Int)
    mt = merkle $ Leaf 69

-- MAIN
main :: IO ()
main = defaultMain
  [ bgroup "Cata Sum Memory"
    [benchCataInt (f i) | i <- [0 .. 15]]
  , bgroup "Generic Cata Sum"
    [benchGenCataSum (f i) | i <- [0 .. 15]]
  , bgroup "Incremental Compute Map"
    [benchIncrementalComputeMap (f i) | i <- [0 .. 15]]
  ]
  where
    f i = round ((10 ** (1 / 3)) ^ i)
