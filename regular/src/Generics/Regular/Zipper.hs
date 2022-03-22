{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-
  Copied from package transformations-0.2.0.0
-}

module Generics.Regular.Zipper
  (-- * Locations
   Loc(..),
   -- * Context frames
   Ctx(..),
   -- * Generic zipper class
   Zipper(..),
   -- * Interface
   enter,
   down, down', up, right, left,
   -- dfnext, dfprev,
   leave, on, update, updateM

  )
  where

import           Prelude             hiding (last)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe

import           Generics.Regular    hiding (left, right)

-- * Locations and context stacks

-- | Abstract type of locations. A location contains the current focus
-- and its context. A location is parameterized over the family of
-- datatypes and over the type of the complete value.

data Loc :: * -> * where
  Loc :: (Regular a, Zipper (PF a)) => a -> [Ctx (PF a) a] -> Loc a

-- * Context frames

-- | Abstract type of context frames. Not required for the high-level
-- navigation functions.

data family Ctx (f :: * -> *) :: * -> *

data instance Ctx (K a)     r
data instance Ctx U         r
data instance Ctx (f :+: g) r = CL (Ctx f r)
                              | CR (Ctx g r)
data instance Ctx (f :*: g) r = C1 (Ctx f r) (g r)
                              | C2 (f r) (Ctx g r)
data instance Ctx I         r = CId
data instance Ctx (C c f)   r = CC (Ctx f r)
data instance Ctx (S s f)   r = CS (Ctx f r)

-- * Contexts and locations are functors

instance Zipper f => Functor (Ctx f) where
  fmap = cmap

-- instance Functor (Loc f) where
  -- fmap f (Loc p x)  = Loc (f p) (map (fmap f) x)

-- * Generic navigation functions

-- | It is in general not necessary to use the generic navigation
-- functions directly. The functions listed in the ``Interface'' section
-- below are more user-friendly.
--

class Functor f => Zipper f where
  cmap        :: (a -> b) -> Ctx f a -> Ctx f b
  fill        :: Ctx f a -> a -> f a
  first, last :: f a -> Maybe (a, Ctx f a)
  next, prev  :: Ctx f a -> a -> Maybe (a, Ctx f a)

instance Zipper I where
  cmap  f CId = CId
  fill  CId x = I x
  first (I x) = Just (x, CId)
  last  (I x) = Just (x, CId)
  next  CId x = Nothing
  prev  CId x = Nothing

instance Zipper (K a) where
  cmap f void = impossible void
  fill void x = impossible void
  first (K a) = Nothing
  last  (K a) = Nothing
  next  void x = impossible void
  prev  void x = impossible void

instance Zipper U where
  cmap f void = impossible void
  fill void x = impossible void
  first U      = Nothing
  last  U      = Nothing
  next  void x = impossible void
  prev  void x = impossible void

instance (Zipper f, Zipper g) => Zipper (f :+: g) where
  cmap f (CL c) = CL (cmap f c)
  cmap f (CR c) = CR (cmap f c)
  fill (CL c) x = L (fill c x)
  fill (CR c) y = R (fill c y)
  first (L x) = first x >>= return . fmap CL
  first (R x) = first x >>= return . fmap CR
  last  (L x) = last x >>= return . fmap CL
  last  (R x) = last x >>= return . fmap CR
  next  (CL c) x = next c x >>= return . fmap CL
  next  (CR c) x = next c x >>= return . fmap CR
  prev  (CL c) x = prev c x >>= return . fmap CL
  prev  (CR c) x = prev c x >>= return . fmap CR

instance (Zipper f, Zipper g) => Zipper (f :*: g) where
  cmap f (C1 c y) = C1 (cmap f c) (fmap f y)
  cmap f (C2 x c) = C2 (fmap f x) (cmap f c)
  fill (C1 c y) x = fill c x :*: y
  fill (C2 x c) y = x :*: fill c y
  first (x :*: y) =         fmap (fmap (flip C1 y)) (first x)
                    `mplus` fmap (fmap (C2 x))      (first y)
  last  (x :*: y) =         fmap (fmap (C2 x))      (last  y)
                    `mplus` fmap (fmap (flip C1 y)) (last  x)
  next (C1 c y) z =         (fmap (flip C1 y)     <$> next c z)
                    `mplus` (fmap (C2 (fill c z)) <$> first y)
  next (C2 x c) z =          fmap (C2 x)          <$> next c z
  prev (C1 c y) z =          fmap (flip C1 y)     <$> prev c z
  prev (C2 x c) z =         (fmap (C2 x)               <$> prev c z)
                    `mplus` (fmap (flip C1 (fill c z)) <$> last x)

instance (Zipper f) => Zipper (C c f) where
  cmap f (CC c)   = CC (cmap f c)
  fill   (CC c) x = C (fill c x)
  first  (C x)    = first  x >>= return . fmap CC
  last   (C x)    = last   x >>= return . fmap CC
  next   (CC c) x = next c x >>= return . fmap CC
  prev   (CC c) x = prev c x >>= return . fmap CC

instance (Zipper f) => Zipper (S s f) where
  cmap f (CS c)   = CS (cmap f c)
  fill   (CS c) x = S (fill c x)
  first  (S x)    = first  x >>= return . fmap CS
  last   (S x)    = last   x >>= return . fmap CS
  next   (CS c) x = next c x >>= return . fmap CS
  prev   (CS c) x = prev c x >>= return . fmap CS

-- * Interface

-- ** Introduction

-- | Start navigating a datastructure. Returns a location that
-- focuses the entire value and has an empty context.
enter :: (Regular a, Zipper (PF a)) => a -> Loc a
enter x = Loc x []

-- ** Navigation

-- | Move down to the leftmost child. Returns 'Nothing' if the
-- current focus is a leaf.
down :: Loc a -> Maybe (Loc a)
down (Loc x cs) = first (from x) >>= \(a,c) -> return (Loc a (c:cs))

-- | Move down to the rightmost child. Returns 'Nothing' if the
-- current focus is a leaf.
down' :: Loc a -> Maybe (Loc a)
down' (Loc x cs) = last (from x) >>= \(a,c) -> return (Loc a (c:cs))

-- | Move up to the parent. Returns 'Nothing' if the current
-- focus is the root.
up :: Loc a -> Maybe (Loc a)
up (Loc x [])     = Nothing
up (Loc x (c:cs)) = return (Loc (to (fill c x)) cs)

-- | Move to the right sibling. Returns 'Nothing' if the current
-- focus is the rightmost sibling.
right :: Loc a -> Maybe (Loc a)
right (Loc x []    ) = Nothing
right (Loc x (c:cs)) = next c x >>= \(a,c') -> return (Loc a (c':cs))

-- | Move to the left sibling. Returns 'Nothing' if the current
-- focus is the leftmost sibling.
left :: Loc a -> Maybe (Loc a)
left (Loc x []    ) = Nothing
left (Loc x (c:cs)) = prev c x >>= \(a,c') -> return (Loc a (c':cs))


-- ** Derived navigation.
{-
df :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
df d u lr l =
  case d l of
    Nothing -> df' l
    r       -> r
 where
  df' l =
    case lr l of
      Nothing -> case u l of
                   Nothing -> Nothing
                   Just l' -> df' l'
      r       -> r

-- | Move through all positions in depth-first left-to-right order.
dfnext :: Loc phi I0 ix -> Maybe (Loc phi I0 ix)
dfnext = df down up right

-- | Move through all positions in depth-first right-to-left order.
dfprev :: Loc phi I0 ix -> Maybe (Loc phi I0 ix)
dfprev = df down' up left
-}

-- | Utility
-- navigate :: (Regular a, Zipper (PF a))
         -- => a -> (Loc a -> Maybe (Loc a)) -> Loc a
-- navigate a f = fromJust $ f (enter a)

-- ** Elimination

-- | Return the entire value, independent of the current focus.
leave :: Loc a -> a
leave (Loc x []) = x
leave loc        = leave (fromJust (up loc))

-- | Operate on the current focus. This function can be used to
-- extract the current point of focus.
on :: Loc a -> a
on (Loc x _) = x

-- | Update the current focus without changing its type.
update :: (a -> a) -> Loc a -> Loc a
update f (Loc x cs) = Loc (f x) cs

-- | Update the current focus without changing its type.
updateM :: Monad m => (a -> m a) -> Loc a -> m (Loc a)
updateM f (Loc x cs) = f x >>= \y -> return (Loc y cs)

-- * Internal functions

impossible :: a -> b
impossible x = x `seq` error "impossible"
