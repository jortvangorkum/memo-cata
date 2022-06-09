{-# LANGUAGE RecordWildCards #-}

module MultiIter
  ( multiIterBenches
  ) where

import           Control.Monad              (replicateM)
import           Criterion.Main
import           Data.ByteString            (ByteString)
import qualified Data.HashMap.Strict        as H
import qualified Data.Map                   as M
import           Environments
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import qualified Generics.Memo.Container    as C
import           Generics.Memo.Main
import           Generics.Memo.Zipper
import           Test.QuickCheck
import           Utils

-- MEMORY USAGE
type Cache   = H.HashMap ByteString Int
type CataSum = Cache -> MerklePF (Tree Int) -> (Int, Cache)

benchIter :: Int -> CataSum -> EnvIter -> Benchmark
benchIter n f envIter = bench (show n) . nf (fst . applyChanges (curContainer envIter) (curTree envIter)) $ changes envIter
  where
    applyChanges :: Cache -> MerklePF (Tree Int) -> Changes -> (Int, Cache)
    applyChanges m t [Change ds rt] = y
      where
        y  = f m t'
        t' = update rt ds t
    applyChanges m t ((Change ds rt):cs) = y'
      where
        t'        = update rt ds t
        y@(z, m') = f m t'
        y'        = applyChanges m' t' cs

benchCataIter :: Int -> IO EnvIter -> Benchmark
benchCataIter n cs = env cs $ benchIter n (\_ t -> (cataInt t, H.empty))

benchGenCataIter :: Int -> IO EnvIter -> Benchmark
benchGenCataIter n cs = env cs $ benchIter n (\_ t -> cataSum t)

benchIncCataIter :: Int -> IO EnvIter -> Benchmark
benchIncCataIter n cs = env cs $ benchIter n cataSumMap

multiIterBenches :: ConfigIter -> Benchmark
multiIterBenches (ConfigIter {..}) = bgroup ("Multi Iterations/" ++ show confScenario ++ "/" ++ show confNIters)
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
