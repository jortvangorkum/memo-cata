{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
module Tree where

import           Generics
import           Generics.Data.Digest.CRC32

type TreeF a = Fix (TreeFr a)

data TreeFr a r = LeafF a
               | NodeF r a r
  deriving (Functor)

class MerkelizeF f where
  merkleF :: Fix f -> Fix (f :*: K Digest)

instance (Show a) => MerkelizeF (TreeFr a) where
  merkleF (In (LeafF n)) = In $ Pair (LeafF n, K h)
    where
      h = digestConcat [digest "LeafF", digest n]
  merkleF (In (NodeF l n r)) = In $ Pair (NodeF l' n r', K h)
    where
      l' = merkleF l
      r' = merkleF r
      dig (In (Pair (x, K xh))) = xh
      hl = dig l'
      hr = dig r'
      h = digestConcat [digest "NodeF", digest n, hl, hr]

exampleTreeF :: TreeF Int
exampleTreeF = In $ NodeF (In (LeafF 1)) 2 (In (LeafF 3))

foldTree :: TreeF Int -> Int
foldTree = cata (\case
  LeafF n     -> n
  NodeF n i j -> n + i + j)

foldMerkle :: TreeF Int -> [Digest]
foldMerkle x = cata f mt
  where
    mt = merkleF x
    f (Pair (px, K h)) = case px of
      LeafF n       -> [h]
      NodeF hl n hr -> h : hl ++ hr
