{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Zipper.Monad where

import           Control.Monad.State

{- ZIPPER MONAD -}

data Loc a c = Loc { struct :: a
                   , cxt    :: c }
             deriving (Show, Eq)

newtype Travel loc a = Travel { unT :: State loc a }
  deriving (Functor, Applicative, Monad, MonadState loc)

traverse :: Loc a c
         -> Travel (Loc a c) a
         -> a
traverse start tt = evalState (unT tt) start

modifyStruct :: (a -> a) -> Travel (Loc a c) a
modifyStruct f = modify editStruct >> gets struct
  where
    editStruct (Loc s c) = Loc (f s) c

putStruct :: a -> Travel (Loc a c) a
putStruct t = modifyStruct $ const t

getStruct :: Travel (Loc a c) a
getStruct = modifyStruct id

{- TREE ZIPPER MONAD -}

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving (Show, Eq)

data Cxt a = Top
           | L (Cxt a) (Tree a)
           | R (Tree a) (Cxt a)
           deriving (Show)

type TreeLoc    a = Loc (Tree a) (Cxt a)
type TravelTree a = Travel (TreeLoc a) (Tree a)

t :: Tree Integer
t = Branch (Branch (Branch (Leaf 1) (Leaf 2))
                   (Leaf 3))
           (Branch (Leaf 4)
                   (Leaf 5))

left :: TravelTree a
left = do
       (Loc (Branch l r) c) <- get
       return l
