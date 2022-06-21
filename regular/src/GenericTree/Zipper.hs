module GenericTree.Zipper where

import           Control.DeepSeq
import           Data.Maybe           (fromJust)
import           GenericTree.Main
import           Generics.Data.Digest
import           Generics.Data.Dirs

data Cxt a = L a (Tree a)
           | R a (Tree a)

type Zipper a = (Tree a, [Cxt a])

-- Construction and Destruction

enter :: Tree a -> Zipper a
enter x = (x, [])

leave :: Zipper a -> Tree a
leave (x, _) = x

-- Traversal

down :: Zipper a -> Maybe (Zipper a)
down (Node l x r, cs) = Just (l, L x r:cs)
down _                = Nothing

down' :: Zipper a -> Maybe (Zipper a)
down' (Node l x r, cs) = Just (r, R x l:cs)
down' _                = Nothing

up :: Zipper a -> Maybe (Zipper a)
up (t, L x r:cs) = Just (Node t x r, cs)
up (t, R x l:cs) = Just (Node l x t, cs)
up _             = Nothing

bottom :: Zipper a -> Maybe (Zipper a)
bottom loc = case down loc of
  Nothing   -> Just loc
  Just loc' -> bottom loc'

bottom' :: Zipper a -> Maybe (Zipper a)
bottom' loc = case down' loc of
  Nothing   -> Just loc
  Just loc' -> bottom' loc'

top :: Zipper a -> Zipper a
top x@(t, []) = x
top x         = top (fromJust (up x))

-- Conditions

isTop :: Zipper a -> Bool
isTop (_, []) = True
isTop _       = False

-- Modifications

modify :: (Tree a -> Tree a) -> Zipper a -> Zipper a
modify f (x, cs) = (f x , cs)

update :: (Tree a -> Tree a) -> Dirs -> Tree a -> Tree a
update f dirs = leave . top . modify f . applyDirs dirs . enter

-- Directions

applyDir :: Dir -> Zipper a -> Maybe (Zipper a)
applyDir Up    = up
applyDir Bttm  = bottom
applyDir Bttm' = bottom'
applyDir Dwn   = down
applyDir Dwn'  = down'

applyDirs :: Dirs -> Zipper a -> Zipper a
applyDirs dirs x = foldl (\y d -> fromJust (applyDir d y)) x dirs
