module Helper.Arbitrary where

import           Test.QuickCheck

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)

