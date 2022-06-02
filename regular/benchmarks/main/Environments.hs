{-# LANGUAGE RecordWildCards #-}

module Environments where

import           Control.Monad              (replicateM)
import           Criterion.Main
import           Data.ByteString            (ByteString)
import qualified Data.HashMap.Strict        as H
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

setupMapInt :: Int -> IO (H.HashMap ByteString Int, MerklePF (Tree Int))
setupMapInt n = do t <- setupMerkleTree n
                   let m = snd $ cataSum t
                   return (m, t)

setupWorstCase :: Int -> Changes
setupWorstCase nIters = [Change [Bttm] (const (merkle (Leaf i))) | i <- [1..nIters]]

setupAverageCase :: Int -> Int -> Changes
setupAverageCase nIters n = [Change (replicate na Dwn) (const (merkle (Leaf i))) | i <- [1..nIters]]
  where
    na :: Int
    na = round ((logBase 2.0 (fromIntegral n)) / 2.0)

setupBestCase :: Int -> Changes
setupBestCase nIters = [Change [Dwn'] (const (merkle (Leaf i))) | i <- [1..nIters]]

setupIter :: ConfigEnv -> IO EnvIter
setupIter (ConfigEnv {..}) = do mt <- setupMerkleTree nNodes
                                let m = snd $ cataSum mt
                                return (EnvIter mt m cs)
  where
    cs = case scenario of
      Worst   -> setupWorstCase nIters
      Average -> setupAverageCase nIters nNodes
      Best    -> setupBestCase nIters
