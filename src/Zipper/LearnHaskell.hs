module Zipper.LearnHaskell where

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

t :: Tree Char
t =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

data Cxt a = Top
           | L a (Tree a) (Cxt a)
           | R (Tree a) a (Cxt a)
           deriving (Show)

type Zipper a = (Tree a, Cxt a)

left :: Zipper a -> Zipper a
left (Node x l r, c) = (l, L x r c)

right :: Zipper a -> Zipper a
right (Node x l r, c) = (r, R l x c)

up :: Zipper a -> Zipper a
up (t, L x r c) = (Node x t r, c)
up (t, R l x c) = (Node x l t, c)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, c) = (Node (f x) l r, c)
modify f (Empty, c)      = (Empty, c)

top :: Zipper a -> Zipper a
top z@(t, Top) = z
top z          = top (up z)
