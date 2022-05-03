{-# LANGUAGE RecordWildCards #-}

module Benchmarks.MultiIter
  ( multiIterBenches
  ) where

import           Control.Monad              (replicateM)
import           Criterion.Main
import qualified Data.Map                   as M
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Memo.Zipper
import           Test.QuickCheck
import           Utils

-- ENVIRONMENTS
setupMerkleTree :: Int -> IO (MerklePF (Tree Int))
setupMerkleTree = return . merkle . generateTree

setupDirs :: Int -> IO Dirs
setupDirs n = sequence [generate genDir | _ <- [0 .. n]]
  where
    genDir = elements [Up, Dwn, Dwn', Lft, Rght, Bttm, Bttm']

setupIter :: ConfigIter -> IO (EnvIter)
setupIter (ConfigIter {..}) =
  do cs <- genCS
     mt <- setupMerkleTree nNodes
     return (EnvIter cs mt)
  where
    genCS = replicateM nIters $
      do ds <- setupDirs nDirs
         rt <- setupMerkleTree 1
         return (Change rt ds)

-- MEMORY USAGE
type Cache   = M.Map Digest Int
type CataSum = Cache -> MerklePF (Tree Int) -> (Int, Cache)

benchIter :: ConfigIter -> CataSum -> EnvIter -> Benchmark
benchIter (ConfigIter {nNodes = n}) f = bench (show n) . nf (applyChanges M.empty)
  where
    applyChanges :: Cache -> EnvIter -> Int
    applyChanges m (EnvIter [Change rt ds] t) = fst $ f m t'
      where
        t' = update' (const rt) ds t
    applyChanges m (EnvIter ((Change rt ds):cs) t) = y
      where
        t'      = update' (const rt) ds t
        (z, m') = f m t'
        y       = z `seq` applyChanges m' (EnvIter cs t')

benchCataIter :: ConfigIter -> IO (EnvIter) -> Benchmark
benchCataIter config cs = env cs $ benchIter config (\_ t -> (cataInt t, undefined))

benchGenCataIter :: ConfigIter -> IO (EnvIter) -> Benchmark
benchGenCataIter config cs = env cs $ benchIter config (\_ t -> cataSum t)

benchIncCataIter :: ConfigIter -> IO (EnvIter) -> Benchmark
benchIncCataIter config cs = env cs $ benchIter config cataSumMap

multiIterBenches :: ConfigIter -> Benchmark
multiIterBenches config = bgroup ("Multi Iterations " ++ show its)
                          [ bgroup "Cata Sum Without Cache" $
                            zipWith benchCataIter configs envs
                          , bgroup "Cata Sum Only Empty Cache" $
                            zipWith benchGenCataIter configs envs
                          , bgroup "Incremental Cata Sum" $
                            zipWith benchIncCataIter configs envs
                          ]
  where
    its = nIters config
    n = nNodes config
    n' = fromIntegral n
    f i = round ((10 ** (1 / (n' / 5))) ^ i)
    is = [f i | i <- [0 .. n]]

    configs = map (ConfigIter 10 10) is
    envs = map setupIter configs
