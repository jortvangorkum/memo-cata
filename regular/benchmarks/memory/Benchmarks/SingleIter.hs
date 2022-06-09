module Benchmarks.SingleIter
  ( singleIterBenches
  ) where

import           Criterion.Main
import           Environments
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Memo.Zipper

benchCataInt :: Int -> Benchmark
benchCataInt n = env (return . generateTree $ n) (bench (show n) . nf cataSumTree)

benchGenCataSum :: Int -> Benchmark
benchGenCataSum n = env (setupMerkleTree n) (bench (show n) . nf cataSum)

benchIncrementalComputeMap :: Int -> Benchmark
benchIncrementalComputeMap n = env (setupMapInt n) (bench (show n) . nf (\(m, t) -> fst $ cataSumMap m (update (const mt) [Bttm] t)))
  where
    mt :: MerklePF (Tree Int)
    mt = merkle $ Leaf 69

singleIterBenches :: [Int] -> Benchmark
singleIterBenches ns = bgroup "Single Iteration"
                        [ bgroup "Cata Sum" $ map benchCataInt ns
                        , bgroup "Generic Cata Sum" $ map benchGenCataSum ns
                        , bgroup "Incremental Cata Sum" $ map benchIncrementalComputeMap ns
                        ]
