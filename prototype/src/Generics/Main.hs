{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Generics.Main where

import           Control.DeepSeq
import           Data.ByteString            (ByteString)
import qualified Data.Map                   as M
import qualified Data.Trie                  as T
import qualified Data.Trie.Internal         as TI
import           Generics.Data.Digest.CRC32

newtype Fix f = In { unFix :: f (Fix f) }

instance Eq (f (Fix f)) => Eq (Fix f) where
  f == g = unFix f == unFix g

instance Show (f (Fix f)) => Show (Fix f) where
  show = show . unFix

newtype I r         = I r                   deriving (Show, Eq)
newtype K a r       = K a                   deriving (Show, Eq)
data (:+:) f g r    = Inl (f r) | Inr (g r) deriving (Show, Eq)
newtype (:*:) f g r = Pair (f r, g r)       deriving (Show, Eq)

infixr 7 :*:
infixr 6 :+:

-- Generic Functors
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (Pair (x, y)) = Pair (fmap f x, fmap f y)

instance Functor I where
  fmap f (I x) = I (f x)

instance Functor (K a) where
  fmap _ (K x) = K x

-- Generic Foldable
-- https://github.com/blamario/grampa/blob/f4b97674161c6bd5e45c20226b5fb3458f942ff4/rank2classes/src/Rank2.hs#L307
instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldMap f (Inl x) = foldMap f x
  foldMap f (Inr x) = foldMap f x

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
  foldMap f (Pair (x, y)) = foldMap f x <> foldMap f y

instance Foldable (K a) where
  foldMap _ _ = mempty

instance Foldable I where
  foldMap f (I r) = f r

-- Generic Traversable
-- https://www.tweag.io/blog/2021-07-08-linear-traversable/
instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse f (Inl x) = Inl <$> traverse f x
  traverse f (Inr x) = Inr <$> traverse f x

instance (Traversable f, Traversable g) => Traversable (f :*: g) where
  traverse f (Pair (x, y)) = curry Pair <$> traverse f x <*> traverse f y

instance Traversable (K a) where
  traverse f (K x) = pure (K x)

instance Traversable I where
  traverse f (I r) = I <$> f r

-- Digest NFData

instance NFData Digest where
  rnf = rwhnf

-- Generic NFData
-- https://hackage.haskell.org/package/deepseq-1.4.6.1/docs/src/Control.DeepSeq.html#line-535
instance NFData (f (Fix f)) => NFData (Fix f) where
  rnf (In x) = rnf x

instance NFData r => NFData (I r) where
  rnf = rnf1

instance NFData1 I where
  liftRnf f (I x) = f x

instance (NFData a, NFData r) => NFData (K a r) where
  rnf = rnf1

instance NFData a => NFData1 (K a) where
  liftRnf _ (K x) = rwhnf x

instance (NFData1 f, NFData1 g) => NFData1 (f :+: g) where
  liftRnf f (Inl x) = liftRnf f x
  liftRnf f (Inr x) = liftRnf f x

instance (NFData1 f, NFData1 g, NFData r) => NFData ((:+:) f g r) where
  rnf = rnf1

instance (NFData1 f, NFData1 g) => NFData1 (f :*: g) where
  liftRnf f (Pair (x, y)) = liftRnf f x `seq` liftRnf f y

instance (NFData1 f, NFData1 g, NFData r) => NFData ((:*:) f g r) where
  rnf = rnf1

-- Generic Merkelize
class Merkelize f where
  hash    :: f (Fix (g :*: K Digest)) -> Digest
  merkleG :: (Merkelize g) => f (Fix g) -> (f :*: K Digest) (Fix (g :*: K Digest))

instance (Show a) => Merkelize (K a) where
  hash (K x) = digestConcat [digest "K", digest x]
  merkleG k@(K x) = Pair (K x, K h)
    where
      h = hash (K x)

instance Merkelize I where
  hash (I x) = digestConcat [digest "I", ph]
    where
      (In (Pair (_, K ph))) = x
  merkleG (I x) = Pair (I prevX, K h)
    where
      prevX@(In (Pair (_, K ph))) = merkle x
      h = hash (I prevX)

instance (Merkelize f, Merkelize g) => Merkelize (f :+: g) where
  hash (Inl x) = digestConcat [digest "Inl", hash x]
  hash (Inr x) = digestConcat [digest "Inr", hash x]

  merkleG (Inl x) = Pair (Inl prevX, K h)
    where
      (Pair (prevX, K ph)) = merkleG x
      h = digestConcat [digest "Inl", ph]
  merkleG (Inr x) = Pair (Inr prevX, K h)
    where
      (Pair (prevX, K ph)) = merkleG x
      h = digestConcat [digest "Inr", ph]

instance (Merkelize f, Merkelize g) => Merkelize (f :*: g) where
  hash (Pair (x, y)) = digestConcat [digest "Pair", hash x, hash y]

  merkleG (Pair (x, y)) = Pair (Pair (prevX, prevY), K h)
    where
      (Pair (prevX, K phx)) = merkleG x
      (Pair (prevY, K phy)) = merkleG y
      h = digestConcat [digest "Pair", phx, phy]

merkle :: Merkelize f => Fix f -> Fix (f :*: K Digest)
merkle = In . merkleG . unFix

-- Generic Container
class Container c where
  empty  :: c a
  insert :: Digest -> a -> c a -> c a
  lookup :: Digest -> c a -> Maybe a

instance Container (M.Map ByteString) where
  empty  = M.empty
  insert = M.insert . getByteString
  lookup = M.lookup . getByteString

instance Container T.Trie where
  empty  = T.empty
  insert = T.insert . getByteString
  lookup = T.lookup . getByteString

instance NFData a => NFData (T.Trie a) where
  rnf = rnf . f
    where
      f = TI.cata arc br em
      arc _ x _ = rnf x
      br _ _ = ()
      em = ()
