{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module GenericTree.Main where

-- import           Data.Functor.Classes
import qualified Data.Map                   as M
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Cata
import           Generics.Memo.Main
import           Generics.Regular.Base
import           Generics.Regular.TH

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

data RoseTree a = Empty
                | NodeR a (RoseTree a) -- Recursive lists do not work with Generics
                deriving (Show, Eq)

$(deriveAll ''Tree "PFTree")
type instance PF (Tree a) = PFTree a

$(deriveAll ''RoseTree "PFRoseTree")
type instance PF (RoseTree a) = PFRoseTree a

-- https://haskell-explained.gitlab.io/blog/posts/2019/08/27/pattern-synonyms/index.html
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms
-- pattern Tree :: PF (Tree a) -> Tree a
-- pattern Tree a <- PF (Tree a) where
--   Tree (LeafF x)     = L (C (K x))
--   Tree (NodeF l x r) = R (C (I l :*: K x :*: I r))

t :: Tree Int
t = Node (Leaf 1) 2 (Leaf 3)

rt :: RoseTree Int
rt = NodeR 1 (NodeR 2 (NodeR 3 Empty))

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
