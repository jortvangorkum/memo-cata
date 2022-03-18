{-# LANGUAGE TemplateHaskell #-}
module Helper.Arbitrary where

import           GenericTree.Main
import           Test.QuickCheck

instance Arbitrary (Tree Int) where
  arbitrary = do sized (return . generateTreeG)

