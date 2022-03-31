{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module GenericTree.Main where

import           Control.DeepSeq
import qualified Data.Map                   as M
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Cata
import           Generics.Memo.Main
import           Generics.Regular.Base
import           Generics.Regular.TH

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

$(deriveAll ''Tree "PFTree")
type instance PF (Tree a) = PFTree a

t :: Tree Int
t = Node (Leaf 1) 2 (Leaf 3)

-- | Generates Tree of size 2n + 1
generateTree :: Int -> Tree Int
generateTree = generateTreeF
  where
    generateTreeF n = generateBinTree 0 (n - 1)
    generateBinTree :: Int -> Int -> Tree Int
    generateBinTree l u =
      if u < l
      then Leaf l
      else let i = (l + u) `div` 2
           in Node (generateBinTree l (i - 1)) i (generateBinTree (i + 1) u)

instance Foldable Tree where
  foldMap f (Leaf x)     = f x
  foldMap f (Node l x r) = f x `mappend` foldMap f l `mappend` foldMap f r

instance NFData a => NFData (Tree a) where
  rnf (Leaf x)     = rnf x
  rnf (Node l x r) = rnf l `seq` rnf x `seq` rnf r
