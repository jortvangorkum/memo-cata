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

setupMapInt :: Int -> IO (M.Map Digest Int, MerklePF (Tree Int))
setupMapInt n = do t <- setupMerkleTree n
                   let m = snd $ cataSum t
                   return (m, t)

setupDirs :: Int -> IO Dirs
setupDirs n = sequence [generate genDir | _ <- [0 .. n]]
  where
    genDir = elements [Up, Dwn, Dwn', Lft, Rght, Bttm, Bttm']

setupIter :: Int -> Int -> Int -> IO (Changes, BTree)
setupIter nChanges nDirs nNodes = do cs <- changes
                                     mt <- setupMerkleTree nNodes
                                     return (cs, mt)
  where
    changes = replicateM nChanges $
      do ds <- setupDirs nDirs
         rt <- setupMerkleTree 1
         return (rt, ds)

-- BENCHMARKS
benchCataInt :: Int -> Benchmark
benchCataInt n = env (return . generateTree $ n) (bench (show n) . nf cataSumTree)

benchGenCataSum :: Int -> Benchmark
benchGenCataSum n = env (setupMerkleTree n) (bench (show n) . nf cataSum)

benchIncrementalComputeMap :: Int -> Benchmark
benchIncrementalComputeMap n = env (setupMapInt n) (bench (show n) . nf (\(m, t) -> fst $ cataSumMap m (update (const mt) [Bttm] t)))
  where
    mt :: MerklePF (Tree Int)
    mt = merkle $ Leaf 69

type Change  = (MerklePF (Tree Int), Dirs)
type Changes = [Change]
type BTree   = MerklePF (Tree Int)
type Cache   = M.Map Digest Int

benchIter :: Int -> (Cache -> BTree -> (Int, Cache)) -> (Changes, BTree) -> Benchmark
benchIter nNodes f = bench (show nNodes) . nf (uncurry (applyChanges M.empty))
  where
    applyChanges :: Cache -> Changes -> BTree -> Int
    applyChanges m [(rt, ds)] t = fst $ f m t'
      where
        t' = update' (const rt) ds t
    applyChanges m ((rt, ds):cs) t = y
      where
        (z, m') = f m t'
        y = z `seq` applyChanges m' cs t'
        t' = update' (const rt) ds t

benchCataIter :: Int -> IO (Changes, BTree) -> Benchmark
benchCataIter nNodes cs = env cs $ benchIter nNodes (\_ t -> (cataInt t, undefined))

benchGenCataIter :: Int -> IO (Changes, BTree) -> Benchmark
benchGenCataIter nNodes cs = env cs $ benchIter nNodes (\_ t -> cataSum t)

benchIncCataIter :: Int -> IO (Changes, BTree) -> Benchmark
benchIncCataIter nNodes cs = env cs $ benchIter nNodes cataSumMap

map' :: (a -> b) -> [a] -> [(a, b)]
map' f xs = zip xs (map f xs)

-- MAIN
main :: IO ()
main = defaultMain
  [ bgroup "Cata Sum"
    [benchCataInt (f i) | i <- [0 .. 10]]
  , bgroup "Generic Cata Sum"
    [benchGenCataSum (f i) | i <- [0 .. 10]]
  , bgroup "Incremental Compute Map"
    [benchIncrementalComputeMap (f i) | i <- [0 .. 10]]
  , bgroup ("Iterations " ++ show 10 ++ " - Cata Sum Without Cache") $
    map (uncurry benchCataIter) cs
  , bgroup ("Iterations " ++ show 10 ++ " - Cata Sum Only Empty Cache") $
    map (uncurry benchGenCataIter) cs
  , bgroup ("Iterations " ++ show 10 ++ " - Incremental Cata Sum") $
    map (uncurry benchIncCataIter) cs
  ]
  where
    f i = round ((10 ** (1 / 2)) ^ i)
    is = [f i | i <- [0 .. 8]]
    cs = map' (setupIter 10 10) is
