module GenericTree.Zipper where

import           GenericTree.Main
import           Generics.Main

data Cxt f a = Top
             | L f (Cxt f a) a
             | R f (Cxt f a) a
             deriving (Eq, Show)

type Loc f a = (f, Cxt f a)

left :: Loc (TreeG a) a -> Loc (TreeG a) a
left (In (Inr (Pair (Pair (I l, K x), I r))), c) = (l, L r c x)
left (In (Inl _), c) = error "Left on a leaf is not possible"

right :: Loc (TreeG a) a -> Loc (TreeG a) a
right (In (Inr (Pair (Pair (I l, K x), I r))), c) = (r, R l c x)
right (In (Inl _), c) = error "Right on a leaf is not possible"

up :: Loc (TreeG a) a -> Loc (TreeG a) a
up (t, L r c x) = (In (Inr (Pair (Pair (I t, K x), I r))), c)
up (t, R l c x) = (In (Inr (Pair (Pair (I l, K x), I t))), c)

top :: Loc (TreeG a) a -> Loc (TreeG a) a
top z@(_, Top) = z
top z          = top (up z)

modify :: (TreeG a -> TreeG a) -> Loc (TreeG a) a -> Loc (TreeG a) a
modify f (t, c) = (f t, c)
