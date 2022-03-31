{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Helper.Arbitrary where

import           GenericTree.Main
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
