{-# LANGUAGE RecordWildCards #-}

module MultiIter
  ( multiIterBenches
  ) where

import           Control.Monad              (replicateM)
import           Criterion.Main
import qualified Data.Map                   as M
import           Environments
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Memo.Zipper
import           Test.QuickCheck
import           Utils

-- MEMORY USAGE
type Cache   = M.Map Digest Int
type CataSum = Cache -> MerklePF (Tree Int) -> (Int, Cache)

benchIter :: Int -> CataSum -> EnvIter -> Benchmark
benchIter n f = bench (show n) . nf (applyChanges M.empty)
  where
    applyChanges :: Cache -> EnvIter -> (Int, Cache)
    applyChanges m (EnvIter t [Change ds rt]) = y
      where
        y  = f m t'
        t' = update' rt ds t
    applyChanges m (EnvIter t ((Change ds rt):cs)) = y'
      where
        t'        = update' rt ds t
        y@(z, m') = f m t'
        y'        = applyChanges m' (EnvIter t' cs)

benchCataIter :: Int -> IO EnvIter -> Benchmark
benchCataIter n cs = env cs $ benchIter n (\_ t -> (cataInt t, M.empty))

benchGenCataIter :: Int -> IO EnvIter -> Benchmark
benchGenCataIter n cs = env cs $ benchIter n (\_ t -> cataSum t)

benchIncCataIter :: Int -> IO EnvIter -> Benchmark
benchIncCataIter n cs = env cs $ benchIter n cataSumMap

multiIterBenches :: ConfigIter -> Benchmark
multiIterBenches (ConfigIter {..}) = bgroup ("Multi Iterations " ++ show confNIters)
                                     [ bgroup "Cata Sum" $
                                       zipWith benchCataIter nodes envs
                                     , bgroup "Generic Cata Sum" $
                                       zipWith benchGenCataIter nodes envs
                                     , bgroup "Incremental Cata Sum" $
                                       zipWith benchIncCataIter nodes envs
                                     ]
  where
    envConfs = map (ConfigEnv confNIters confScenario) nodes
    envs = map setupIter envConfs
