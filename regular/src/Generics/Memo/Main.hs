{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Generics.Memo.Main where

import           Generics.Data.Digest.CRC32
import           Generics.Regular.Base

class Hashable f where
  hash :: f (Fix (g :*: K Digest)) -> Digest

instance (Show a) => Hashable (K a) where
  hash (K x) = digestConcat [digest "K", digest x]

instance Hashable I where
  hash (I x) = digestConcat [digest "I", getDigest x]
    where
      getDigest :: Fix (f :*: K Digest) -> Digest
      getDigest (In (_ :*: K h)) = h

instance (Hashable f, Hashable g) => Hashable (f :+: g) where
  hash (L x) = digestConcat [digest "L", hash x]
  hash (R x) = digestConcat [digest "R", hash x]

instance (Hashable f, Hashable g) => Hashable (f :*: g) where
  hash (x :*: y) = digestConcat [digest "P", hash x, hash y]

instance (Hashable f) => Hashable (C c f) where
  hash (C x) = digestConcat [digest "C", hash x]

instance Hashable U where
  hash _ = digest "U"

type MerklePF f = Merkle (PF f)
type Merkle f = Fix (f :*: K Digest)

merkle :: (Regular a, Hashable (PF a), Functor (PF a)) => a -> Merkle (PF a)
merkle = In . merkleG . fmap merkle . from

merkleG :: Hashable f => f (Fix (g :*: K Digest)) -> (f :*: K Digest) (Fix (g :*: K Digest))
merkleG f = f :*: K (hash f)

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
