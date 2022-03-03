module Zipper.MerkleTree where


import           GenericTree.Main
import           Generics.Data.Digest.CRC32
import           Generics.Main


data Cxt f a = Top
             | L (Cxt f a) f a Digest
             | R (Cxt f a) f a Digest

type Loc f a = (f, Cxt f a)

left :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
left (In (Pair (Inr (Pair (Pair (I l, K x), I r)), K h)), c) = (l, L c r x h)
left _ = error "Left not possible"

right :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
right (In (Pair (Inr (Pair (Pair (I l, K x), I r)), K h)), c) = (r, R c l x h)
right _ = error "Right not possible"

up :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
up (t, L c r x h) = (In (Pair (Inr (Pair (Pair (I t, K x), I r)), K h)), c)
up (t, R c l x h) = (In (Pair (Inr (Pair (Pair (I l, K x), I t)), K h)), c)
up (t, Top)       = error "Cannot go further than Top"

top :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
top z@(_, Top) = z
top z          = top (up z)

modify :: (MerkleTree a -> MerkleTree a) -> Loc (MerkleTree a) a -> Loc (MerkleTree a) a
modify f (m@(In (Pair (x, K h))), c) = (f m, c)

-- TODO: determine how to rehash the first layer of the fixpoint
-- use that to rehash from the current location to the root node


-- rehash :: Show a => Loc (MerkleTree a) a -> Loc (MerkleTree a) a
-- rehash (In n@(Pair (Inr (Pair (Pair (I l, K x), I r)), K h)), c) = undefined

-- update :: (MerkleTree a -> MerkleTree a) -> Loc (MerkleTree a) a -> Loc (MerkleTree a) a
-- update f ()
