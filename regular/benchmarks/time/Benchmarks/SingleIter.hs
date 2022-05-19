module Benchmarks.SingleIter where

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

singleIterBenches :: Int -> Benchmark
singleIterBenches n = bgroup "Single Iteration"
                        [ bgroup "Cata Sum"
                          [benchCataInt (f i) | i <- [0 .. n]]
                        , bgroup "Generic Cata Sum"
                          [benchGenCataSum (f i) | i <- [0 .. n]]
                        , bgroup "Incremental Cata Sum"
                          [benchIncrementalComputeMap (f i) | i <- [0 .. n]]
                        ]
  where
    n'  = fromIntegral n
    f i = round ((10 ** (1 / (n' / 5))) ^ i)
