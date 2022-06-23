{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Helper.Arbitrary where

import           Data.ByteString      (ByteString)
import qualified Data.HashMap.Strict  as H
import qualified Data.Map             as M
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest
import           Generics.Memo.Main
import           Generics.Memo.Zipper
import           Test.QuickCheck

instance Arbitrary (Tree Int) where
  arbitrary = do sized (return . generateTree)

instance Arbitrary (Merkle (PFTree Int)) where
  arbitrary = do t :: Tree Int <- arbitrary
                 return (merkle t)

instance Arbitrary Dir where
  arbitrary = elements [Up, Dwn, Dwn', Lft, Rght, Bttm, Bttm']

cataIter :: Int -> Gen ((Int, H.HashMap Digest Int), MerklePF (Tree Int))
cataIter 0 = do (t :: MerklePF (Tree Int))  <- arbitrary
                (rt :: MerklePF (Tree Int)) <- arbitrary
                (ds :: Dirs)                <- arbitrary
                let t' = update'' (const rt) ds t
                return (cataSum t', t')
cataIter n = do ((_, m), t) <- cataIter (n - 1)
                (rt :: MerklePF (Tree Int)) <- arbitrary
                (ds :: Dirs)                <- arbitrary
                let t' = update'' (const rt) ds t
                return (cataSumMap m t', t')
