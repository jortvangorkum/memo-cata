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

-- MAIN
main :: IO ()
main = defaultMain
  [ bgroup "Cata Sum Memory"
    [benchCataInt (10 ^ i) | i <- [0, 1, 2, 3, 4, 5]]
  ]
