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
import           Test.QuickCheck
import           Utils

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

setupIter :: ConfigIter -> IO EnvIter
setupIter (ConfigIter {..}) =
  do cs <- genCS
     mt <- setupMerkleTree nNodes
     return (EnvIter cs mt)
  where
    genCS = replicateM nIters $
      do ds <- setupDirs nDirs
         rt <- setupMerkleTree 1
         return (Change rt ds)
