{-# LANGUAGE RecordWildCards #-}

module Environments where

import           Control.Monad              (replicateM)
import           Criterion.Main
import qualified Data.Map                   as M
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Memo.Zipper
import           Generics.Regular.Base
import           Test.QuickCheck
import           Utils

setupMerkleTree :: Int -> IO (MerklePF (Tree Int))
setupMerkleTree = return . merkle . generateTree

setupMapInt :: Int -> IO (M.Map Digest Int, MerklePF (Tree Int))
setupMapInt n = do t <- setupMerkleTree n
                   let m = snd $ cataSum t
                   return (m, t)

setupWorstCase :: Int -> Int -> IO EnvIter
setupWorstCase nIters n = do mt <- setupMerkleTree n
                             return (EnvIter mt cs)
  where
    cs :: Changes
    cs = [Change [Bttm] (const (merkle (Leaf i))) | i <- [1..nIters]]

setupAverageCase :: Int -> Int -> IO EnvIter
setupAverageCase nIters n = do mt <- setupMerkleTree n
                               return (EnvIter mt cs)
  where
    cs :: Changes
    cs = [Change (replicate na Dwn) (const (merkle (Leaf i))) | i <- [1..nIters]]
    na :: Int
    na = round ((logBase 2.0 (fromIntegral n)) / 2.0)

setupBestCase :: Int -> Int -> IO EnvIter
setupBestCase nIters n = do mt <- setupMerkleTree n
                            return (EnvIter mt cs)
  where
    cs :: Changes
    cs = [Change [] (removeRightSide i) | i <- [1..nIters]]
    removeRightSide :: Int -> MerklePF (Tree Int) -> MerklePF (Tree Int)
    removeRightSide i (In ((R (C (I l :*: K x :*: I r))) :*: K h)) = (In ((R (C (I l :*: K x :*: I (merkle (Leaf i))))) :*: K h))

setupIter :: ConfigEnv -> IO EnvIter
setupIter (ConfigEnv {..}) = case scenario of
  Worst   -> setupWorstCase nIters nNodes
  Average -> setupAverageCase nIters nNodes
  Best    -> setupBestCase nIters nNodes
