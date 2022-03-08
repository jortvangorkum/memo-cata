{-# LANGUAGE FlexibleInstances #-}
module Test.Arbitrary where
import           GenericTree.Main
import           Generics.Main
import           Test.QuickCheck

instance Arbitrary (TreeG Int) where
  arbitrary = do sized (return . generateTreeG)

instance Arbitrary (MerkleTree Int) where
  arbitrary = merkle <$> arbitrary
