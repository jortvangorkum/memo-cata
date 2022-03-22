{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Helper.Arbitrary where

import           GenericTree.Main
import           Generics.Memo.Main
import           Test.QuickCheck

instance Arbitrary (Tree Int) where
  arbitrary = do sized (return . generateTree)

instance Arbitrary (MerkleTree Int) where
  arbitrary = do t :: Tree Int <- arbitrary
                 return (merkle t)
