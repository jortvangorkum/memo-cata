{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Generics.Memo.Main where

import           Generics.Data.Digest.CRC32
import           Generics.Regular.Base

class Merkelize f where
  merkleG :: f (Fix (g :*: K Digest)) -> (f :*: K Digest) (Fix (g :*: K Digest))

type Merkle f = Fix (PF f :*: K Digest)

merkle :: (Regular a, Merkelize (PF a), Functor (PF a)) => a -> Merkle a
merkle = In . merkleG . fmap merkle . from

instance (Show a) => Merkelize (K a) where
  merkleG (K x) = K x :*: K h
    where
      h = digestConcat [digest "K", digest x]

instance Merkelize I where
  merkleG (I x) = I x :*: K h
    where
      (In (_ :*: K ph)) = x
      h = digestConcat [digest "I", ph]

instance (Merkelize f, Merkelize g) => Merkelize (f :+: g) where
  merkleG (L x) = L prevX :*: K h
    where
      (prevX :*: K ph) = merkleG x
      h = digestConcat [digest "L", ph]
  merkleG (R x) = R prevX :*: K h
    where
      (prevX :*: K ph) = merkleG x
      h = digestConcat [digest "R", ph]

instance (Merkelize f, Merkelize g) => Merkelize (f :*: g) where
  merkleG (x :*: y) = (prevX :*: prevY) :*: K h
    where
      prevX :*: K phx = merkleG x
      prevY :*: K phy = merkleG y
      h = digestConcat [digest "Pair", phx, phy]

instance (Merkelize f) => Merkelize (C c f) where
  merkleG (C x) = C x :*: K h
    where
      h = digestConcat [digest "C", ph]
      prevX :*: K ph = merkleG x

instance Merkelize U where
  merkleG U = U :*: K h
    where
      h = digest "U"

-- Generic Foldable
-- https://github.com/blamario/grampa/blob/f4b97674161c6bd5e45c20226b5fb3458f942ff4/rank2classes/src/Rank2.hs#L307
instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldMap f (L x) = foldMap f x
  foldMap f (R x) = foldMap f x

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
  foldMap f (x :*: y) = mappend (foldMap f x) (foldMap f y)

instance Foldable (K a) where
  foldMap _ _ = mempty

instance Foldable I where
  foldMap f (I r) = f r

instance Foldable f => Foldable (C c f) where
  foldMap f (C x) = foldMap f x

instance Foldable U where
  foldMap _ _ = mempty

-- Generic Traversable
-- https://www.tweag.io/blog/2021-07-08-linear-traversable/
instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse f (L x) = L <$> traverse f x
  traverse f (R x) = R <$> traverse f x

instance (Traversable f, Traversable g) => Traversable (f :*: g) where
  traverse f (x :*: y) = (:*:) <$> traverse f x <*> traverse f y

instance Traversable (K a) where
  traverse f (K x) = pure (K x)

instance Traversable I where
  traverse f (I r) = I <$> f r

instance Traversable f => Traversable (C c f) where
  traverse f (C x) = C <$> traverse f x

instance Traversable U where
  traverse f U = pure U
