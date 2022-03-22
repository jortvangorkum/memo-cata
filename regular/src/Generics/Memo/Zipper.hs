{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Generics.Memo.Zipper where

import           Control.Monad              (mplus)
import           Data.Maybe
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Regular.Base
import           Prelude                    hiding (last)

data Loc (a :: *) :: * -> * where
  Loc :: (Regular a, Zipper (PF a)) => Merkle a -> [Ctx (PF a) (Merkle a)] -> Loc a (Merkle a)

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

impossible :: a -> b
impossible x = x `seq` error "impossible"

enter :: (Regular a, Zipper (PF a)) => Merkle a -> Loc a (Merkle a)
enter x = Loc x []

-- leave :: Loc a (Merkle a) -> Merkle a
-- leave (Loc x []) = x
-- leave loc        = leave (fromJust (up loc))

-- up :: Loc a (Merkle a) -> Maybe (Loc a (Merkle a))
-- up (Loc x [])     = Nothing
-- up (Loc x (c:cs)) = Just (Loc (In (fill c x)) cs)
