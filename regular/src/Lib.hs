{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import           Control.Monad.State
import qualified Data.Map                        as M
import           Generics.Data.Digest.CRC32
import           Generics.Regular
import           Generics.Regular.Functions
import qualified Generics.Regular.Functions.Read as G
import qualified Generics.Regular.Functions.Show as G
import           Generics.Regular.TH

data Tree = Leaf Int
          | Node Tree Int Tree
          deriving (Show)

$(deriveAll ''Tree "PFTree")
type instance PF Tree = PFTree

t :: Tree
t = Node (Leaf 1) 2 (Leaf 3)

class Merkelize f where
  merkleG :: f (Fix (g :*: K Digest)) -> (f :*: K Digest) (Fix (g :*: K Digest))

merkle :: (Regular a, Merkelize (PF a), Functor (PF a)) => a -> Fix (PF a :*: K Digest)
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

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg t = alg (fmap (cata alg) (out t))

cataInt :: Fix (PFTree :*: K Digest) -> Int
cataInt = cata f
  where
    f :: (PFTree :*: K Digest) Int -> Int
    f (px :*: K h) = case px of
      L (C (K x))                 -> x
      R (C (I l :*: K x :*: I r)) -> l + x + r

cataHashes :: Fix (PFTree :*: K Digest) -> [Digest]
cataHashes = cata f
  where
    f (px :*: K h) = case px of
      L _                       -> [h]
      R (C (I l :*: _ :*: I r)) -> h : l ++ r

cataMerkleState :: (Functor f, Traversable f)
                => (f a -> a) -> Fix (f :*: K Digest) -> State (M.Map Digest a) a
cataMerkleState alg (In (x :*: K h))
  = do m <- get
       case M.lookup h m of
         Just a -> return a
         Nothing -> do y <- mapM (cataMerkleState alg) x
                       let r = alg y
                       modify (M.insert h r) >> return r

cataMerkle :: (Functor f, Traversable f)
           => (f a -> a) -> Fix (f :*: K Digest) -> (a, M.Map Digest a)
cataMerkle alg t = runState (cataMerkleState alg t) M.empty

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

cataSum :: Fix (PFTree :*: K Digest) -> (Int, M.Map Digest Int)
cataSum = cataMerkle
  (\case
    L (C (K x))                 -> x
    R (C (I l :*: K x :*: I r)) -> l + x + r
  )
