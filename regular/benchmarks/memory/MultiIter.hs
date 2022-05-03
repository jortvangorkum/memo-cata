{-# LANGUAGE RecordWildCards #-}

module MultiIter
  ( multiIterBenches
  ) where

import           Control.DeepSeq
import           Control.Monad              (replicateM)
import           Criterion.Main
import qualified Data.Map                   as M
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Memo.Zipper
import           Test.QuickCheck

-- TYPES
type Changes = [Change]
data Change  = Change
  { newTree    :: MerklePF (Tree Int)
  , directions :: Dirs
  }

data ConfigIter = ConfigIter
  { nIters :: Int
  , nNodes :: Int
  , nDirs  :: Int
  }

data EnvIter = EnvIter
  { changes :: Changes
  , curTree :: MerklePF (Tree Int)
  }

instance NFData EnvIter where
  rnf (EnvIter c t) = c `seq` t `seq` ()

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
multiIterBenches config = bgroup "Multi Iter"
                          [ bgroup ("Iterations " ++ show its ++ " - Cata Sum Without Cache") $
                            zipWith benchCataIter configs envs
                          , bgroup ("Iterations " ++ show its ++ " - Cata Sum Only Empty Cache") $
                            zipWith benchGenCataIter configs envs
                          , bgroup ("Iterations " ++ show its ++ " - Incremental Cata Sum") $
                            zipWith benchIncCataIter configs envs
                          ]
  where
    its = nIters config
    n = fromIntegral (nNodes config)
    f i = round ((n ** (1 / (n / 5))) ^ i)
    is = [f i | i <- [0 .. n]]

    configs = map (ConfigIter 10 10) is
    envs = map setupIter configs
