{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

type family Merkle (f :: (* -> *)) (h :: *) :: (* -> *)

type instance Merkle (K a)     h = K a :*: K h
type instance Merkle I         h = I :*: K h
type instance Merkle (f :+: g) h = (Merkle f h :+: Merkle g h) :*: K h
type instance Merkle (f :*: g) h = (Merkle f h :*: Merkle g h) :*: K h

class Merkelize f where
  merkleG :: Merkelize g => f (Fix g) -> (Merkle f Digest) (Fix (Merkle g Digest))
  -- merkleG :: Merkelize g => f (Fix g) -> (f :*: K Digest) (Fix (g :*: K Digest))

-- merkle :: Merkelize f => Fix f -> Fix (f :*: K Digest)
-- merkle = In . merkleG . out

instance (Show a) => Merkelize (K a) where
  merkleG (K x) = K x :*: K h
    where
      h = digestConcat [digest "K", digest x]

instance Merkelize I where
  merkleG (I x) = I prevX :*: K h
    where
      prevX@(In (_ :*: K ph)) = In . merkleG . out $ x
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
