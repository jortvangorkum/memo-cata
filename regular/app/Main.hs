module Main where

import           Criterion.Main
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Memo.Main
import           Generics.Memo.Zipper

-- BENCHMARKS
benchCataInt :: Int -> Benchmark
benchCataInt n = env (return . generateTree $ n) (bench (show n) . nf cataSumTree)

main :: IO ()
main = defaultMain
  [ bgroup "Cata Sum"
   [benchCataInt (1 * (10 ^ i)) | i <- [0, 1, 2, 3, 4, 5, 6]]
  ]
