module Zipper.Wiki where

data Tree a = Leaf a
            | Fork (Tree a) (Tree a)
            deriving (Eq, Show)

t :: Tree Integer
t = Fork (Fork (Leaf 1) (Leaf 2))
         (Fork (Leaf 3) (Leaf 4))

-- Context
data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a)
  deriving (Eq, Show)

-- Location
type Loc a = (Tree a, Cxt a)

-- Location Manipulations
left :: Loc a -> Loc a
left (Fork l r, c) = (l, L c r)

right :: Loc a -> Loc a
right (Fork l r, c) = (r, R l c)

top :: Tree a -> Loc a
top t = (t, Top)

up :: Loc a -> Loc a
up (t, L c r) = (Fork t r, c)
up (t, R l c) = (Fork l t, c)

upmost :: Loc a -> Loc a
upmost l@(t, Top) = l
upmost l          = upmost (up l)

modify :: Loc a -> (Tree a -> Tree a) -> Loc a
modify (t, c) f = (f t, c)

insertRight :: Loc a -> Tree a -> Loc a
insertRight (Fork l _, c) t = (Fork l t, c)

insertLeft :: Loc a -> Tree a -> Loc a
insertLeft (Fork _ r, c) t = (Fork t r, c)
