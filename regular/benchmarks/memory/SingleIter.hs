module SingleIter
  ( singleIterBenches
  ) where

import           Criterion.Main
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Memo.Zipper

benchCataInt :: Int -> Benchmark
benchCataInt n = bench (show n) $ nf (cataSumTree . generateTree) n

benchGenCataSum :: Int -> Benchmark
benchGenCataSum n = bench (show n) $ nf (cataSum . merkle . generateTree) n

benchIncrementalComputeMap :: Int -> Benchmark
benchIncrementalComputeMap n = bench (show n) $ nf incCataSum n
  where
    incCataSum :: Int -> Int
    incCataSum i = fst $ cataSumMap m (update (const mt) [Bttm] t)
      where
        t = merkle $ generateTree i
        m = snd $ cataSum t
    mt :: MerklePF (Tree Int)
    mt = merkle $ Leaf 69

singleIterBenches :: Int -> Benchmark
singleIterBenches n = bgroup "Single Iter"
                        [ bgroup "Cata Sum"
                          [benchCataInt (f i) | i <- [0 .. n]]
                        , bgroup "Generic Cata Sum"
                          [benchGenCataSum (f i) | i <- [0 .. n]]
                        , bgroup "Incremental Compute Map"
                          [benchIncrementalComputeMap (f i) | i <- [0 .. n]]
                        ]
  where
    n'  = fromIntegral n
    f i = round ((10 ** (1 / (n' / 5))) ^ i)
