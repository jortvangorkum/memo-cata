module Main where

import           Control.Monad              (replicateM)
import           Criterion.Main
import qualified Data.Map                   as M
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Memo.Zipper
import           Test.QuickCheck

-- ENVIRONMENTS
setupMerkleTree :: Int -> IO (MerklePF (Tree Int))
setupMerkleTree = return . merkle . generateTree

setupDirs :: Int -> IO Dirs
setupDirs n = sequence [generate genDir | _ <- [0 .. n]]
  where
    genDir = elements [Up, Dwn, Dwn', Lft, Rght, Bttm, Bttm']

setupIter :: Int -> Int -> Int -> IO ([(MerklePF (Tree Int), Dirs)], MerklePF (Tree Int))
setupIter nChanges nNodes nDirs = do cs <- changes
                                     mt <- setupMerkleTree nNodes
                                     return (cs, mt)
  where
    changes = replicateM nChanges $
      do ds <- setupDirs nDirs
         rt <- setupMerkleTree 1
         return (rt, ds)

-- MEMORY USAGE
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

benchCataIter :: Int -> Int -> Benchmark
benchCataIter nChange nNodes = env (setupIter nChange nNodes nNodes) (bench (show nNodes) . nf applyChanges)
  where
    applyChanges :: ([(MerklePF (Tree Int), Dirs)], MerklePF (Tree Int)) -> (Int, M.Map Digest Int)
    applyChanges ([(rt, ds)], mt)  = let t' = update' (const rt) ds mt in cataSum t'
    applyChanges ((rt, ds):cs, mt) = cataSumMap m t'
      where
        (_, m) = applyChanges (cs, mt)
        t' = update' (const rt) ds mt


-- MAIN
main :: IO ()
main = defaultMain
  [ bgroup "Cata Sum"
    [benchCataInt (f i) | i <- [0 .. 10]]
  , bgroup "Generic Cata Sum"
    [benchGenCataSum (f i) | i <- [0 .. 10]]
  , bgroup "Incremental Compute Map"
    [benchIncrementalComputeMap (f i) | i <- [0 .. 10]]
  , bgroup ("Iterations " ++ show 10 ++ " - Incremental Compute Map")
    [benchCataIter 10 (f i) | i <- [0 .. 10]]
  ]
  where
    f i = round ((10 ** (1 / 2)) ^ i)
