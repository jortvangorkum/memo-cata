{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

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
