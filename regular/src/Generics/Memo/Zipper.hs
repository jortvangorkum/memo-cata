{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-# HLINT ignore "Use <&>" #-} -- shorthand is introduced in a later base version (4.11.0.0)
{-# HLINT ignore "Use newtype instead of data" #-}

module Generics.Memo.Zipper
  ( Loc(..)
  , Ctx(..)
  , Zipper(..)
  , enter, leave, on
  , up, down, down', left, right, bottom, bottom'
  , modify, update
  ) where

import           Control.Monad              (mplus)
import           Data.Maybe
import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Regular.Base
import           Prelude                    hiding (last)

data Loc :: * -> * where
  Loc :: (Zipper a) => Merkle a -> [Ctx (a :*: K Digest) (Merkle a)] -> Loc (Merkle a)

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

instance Zipper f => Functor (Ctx f) where
  fmap = cmap

-- * Generic navigation functions

-- | It is in general not necessary to use the generic navigation
-- functions directly. The functions listed in the ``Interface'' section
-- below are more user-friendly.

class Functor f => Zipper f where
  cmap        :: (a -> b) -> Ctx f a -> Ctx f b
  fill        :: Ctx f a -> a -> f a
  first, last :: f a -> Maybe (a, Ctx f a)
  next, prev  :: Ctx f a -> a -> Maybe (a, Ctx f a)

instance Zipper I where
  cmap  f CId = CId
  fill  CId   = I
  first (I x) = Just (x, CId)
  last  (I x) = Just (x, CId)
  next  CId x = Nothing
  prev  CId x = Nothing

instance Zipper (K a) where
  cmap f = impossible
  fill void x = impossible void
  first (K a) = Nothing
  last  (K a) = Nothing
  next  void x = impossible void
  prev  void x = impossible void

instance Zipper U where
  cmap f = impossible
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
  first (x :*: y) =         fmap (fmap (`C1` y)) (first x)
                    `mplus` fmap (fmap (C2 x))      (first y)
  last  (x :*: y) =         fmap (fmap (C2 x))      (last  y)
                    `mplus` fmap (fmap (`C1` y)) (last  x)
  next (C1 c y) z =         (fmap (`C1` y)     <$> next c z)
                    `mplus` (fmap (C2 (fill c z)) <$> first y)
  next (C2 x c) z =          fmap (C2 x)          <$> next c z
  prev (C1 c y) z =          fmap (`C1` y)     <$> prev c z
  prev (C2 x c) z =         (fmap (C2 x)               <$> prev c z)
                    `mplus` (fmap (`C1` fill c z) <$> last x)

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

impossible :: a -> b
impossible x = x `seq` error "impossible"

-- ** Construction and Destruction

-- | Start navigating a datastructure. Returns a location that
-- focuses the entire value and has an empty context.
enter :: (Zipper a) => Merkle a -> Loc (Merkle a)
enter x = Loc x []

-- | Return the entire value, independent of the current focus.
leave :: Loc a -> a
leave (Loc x []) = x
leave loc        = leave (fromJust (up loc))

-- | Operate on the current focus. This function can be used to
-- extract the current point of focus.
on :: Loc a -> a
on (Loc x _) = x

-- ** Navigation

-- | Move up to the parent. Returns 'Nothing' if the current
-- focus is the root.
up :: Loc a -> Maybe (Loc a)
up (Loc x [])     = Nothing
up (Loc x (c:cs)) = Just (Loc (In (fill c x)) cs)

-- | Move down to the leftmost child. Returns 'Nothing' if the
-- current focus is a leaf.
down :: Loc a -> Maybe (Loc a)
down (Loc x cs) = first (out x) >>= \(a,c) -> return (Loc a (c:cs))

-- | Move down to the rightmost child. Returns 'Nothing' if the
-- current focus is a leaf.
down' :: Loc a -> Maybe (Loc a)
down' (Loc x cs) = last (out x) >>= \(a,c) -> return (Loc a (c:cs))

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

-- | Move to the leftmost lowest element.
bottom :: Loc a -> Maybe (Loc a)
bottom loc = case down loc of
  Nothing   -> Just loc
  Just loc' -> bottom loc'

-- | Move to the rightmost lowest element.
bottom' :: Loc a -> Maybe (Loc a)
bottom' loc = case down' loc of
  Nothing   -> Just loc
  Just loc' -> bottom' loc'

-- ** Conditions

top :: Loc a -> Bool
top (Loc _ []) = True
top _          = False

-- ** Modification

-- | Modify the current focus without changing its type.
modify :: (a -> a) -> Loc a -> Loc a
modify f (Loc x cs) = Loc (f x) cs

updateLoc :: Hashable a => (Merkle a -> Merkle a) -> Loc (Merkle a) -> Loc (Merkle a)
updateLoc f loc = if   top loc'
                  then loc'
                  else updateParents $ expectJust "Exception: Cannot go up" (up loc')
  where
    loc' = modify f loc

    updateDigest :: Hashable a => Merkle a -> Merkle a
    updateDigest (In (x :*: _)) = In (x :*: K (hash x))

    updateParents :: Hashable a => Loc (Merkle a) -> Loc (Merkle a)
    updateParents (Loc x []) = Loc (updateDigest x) []
    updateParents (Loc x cs) = updateParents
                             $ expectJust "Exception: Cannot go up"
                             $ up (Loc (updateDigest x) cs)

update :: (Zipper a, Hashable a)
        => (Merkle a -> Merkle a)
        -> [Loc (Merkle a) -> Maybe (Loc (Merkle a))]
        -> Merkle a
        -> Merkle a
update f dirs m = leave $ updateLoc f loc'
  where
    loc' = foldl (\x f -> fromMaybe x (f x)) (enter m) dirs

-- TEST INSERT

test :: (Zipper (PF (Tree Int))) => Merkle (PF (Tree Int)) -> Merkle (PF (Tree Int))
test = update (const mt) [down]
  where
    mt :: Merkle (PF (Tree Int))
    mt = merkle $ Leaf 69

-- ** Utility

expectJust :: String -> Maybe a -> a
expectJust msg = fromMaybe (error msg)
