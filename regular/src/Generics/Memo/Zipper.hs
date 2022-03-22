{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Generics.Memo.Zipper where

import           Control.Monad              (mplus)
import           Generics.Data.Digest.CRC32
import           Generics.Memo.Main
import           Generics.Regular.Base
import           Prelude                    hiding (last)

data Loc :: * -> * where
  Loc :: (Regular a, Zipper (PF a)) => Merkle a -> [Ctx (PF a :*: K Digest) a] -> Loc (Merkle a)

data family Ctx (f :: * -> *) :: * -> *

data instance Ctx (K a)     r
data instance Ctx U         r
data instance Ctx (f :+: g) r = CL (Ctx f r)
                              | CR (Ctx g r)
data instance Ctx (f :*: g) r = C1 (Ctx f r) (g r)
                              | C2 (f r) (Ctx g r)
data instance Ctx I         r = CId
newtype instance Ctx (C c f)   r = CC (Ctx f r)
newtype instance Ctx (S s f)   r = CS (Ctx f r)

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
  first (L x) = first x <&> fmap CL
  first (R x) = first x <&> fmap CR
  last  (L x) = last x <&> fmap CL
  last  (R x) = last x <&> fmap CR
  next  (CL c) x = next c x <&> fmap CL
  next  (CR c) x = next c x <&> fmap CR
  prev  (CL c) x = prev c x <&> fmap CL
  prev  (CR c) x = prev c x <&> fmap CR

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
  first  (C x)    = first  x <&> fmap CC
  last   (C x)    = last   x <&> fmap CC
  next   (CC c) x = next c x <&> fmap CC
  prev   (CC c) x = prev c x <&> fmap CC

instance (Zipper f) => Zipper (S s f) where
  cmap f (CS c)   = CS (cmap f c)
  fill   (CS c) x = S (fill c x)
  first  (S x)    = first  x <&> fmap CS
  last   (S x)    = last   x <&> fmap CS
  next   (CS c) x = next c x <&> fmap CS
  prev   (CS c) x = prev c x <&> fmap CS

impossible :: a -> b
impossible x = x `seq` error "impossible"

enter :: (Regular a, Zipper (PF a)) => Merkle a -> Loc (Merkle a)
enter x = Loc x []
