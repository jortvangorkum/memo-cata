module GenericTree.Zipper
  ( left
  , right
  , down
  , down'
  , up
  , top
  , update
  , travel
  , toLoc
  , fromLoc
  , insertTest
  , applyDirs
  ) where

import           Control.DeepSeq
import           Generics.Data.Digest.CRC32
import           Generics.Main              hiding (insert)
import           GenericTree.Main

data Cxt fa a = Top
              | L (Cxt fa a) fa a Digest
              | R (Cxt fa a) fa a Digest
              deriving (Show)

instance (NFData fa, NFData a) => NFData (Cxt fa a) where
  rnf Top         = ()
  rnf (L c t x h) = rnf c `seq` rnf t `seq` rnf x
  rnf (R c t x h) = rnf c `seq` rnf t `seq` rnf x

type Loc fa a = (fa, Cxt fa a)

toLoc :: MerkleTree a -> Loc (MerkleTree a) a
toLoc mt = (mt, Top)

fromLoc :: Loc (MerkleTree a) a -> MerkleTree a
fromLoc (t, _) = t

left :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
left (In (Pair (Inr (Pair (Pair (I l, K x), I r)), K h)), c) = (l, L c r x h)
left _ = error "Left not possible"

right :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
right (In (Pair (Inr (Pair (Pair (I l, K x), I r)), K h)), c) = (r, R c l x h)
right _ = error "Right not possible"

containsChildren :: Loc (MerkleTree a) a -> Bool
containsChildren (In (Pair (Inr _, _)), _) = True
containsChildren _                         = False

-- | Moves down to the leftmost child
down :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
down l@(t, c) = if containsChildren l then down (left l) else l

-- | Moves down to the rightmost child
down' :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
down' l@(t, c) = if containsChildren l then down (right l) else l

up :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
up (t, L c r x h) = (In (Pair (Inr (Pair (Pair (I t, K x), I r)), K h)), c)
up (t, R c l x h) = (In (Pair (Inr (Pair (Pair (I l, K x), I t)), K h)), c)
up (t, Top)       = error "Cannot go further than Top"

top :: Loc (MerkleTree a) a -> Loc (MerkleTree a) a
top z@(_, Top) = z
top z          = top (up z)

isTop :: Loc (MerkleTree a) a -> Bool
isTop (_, Top) = True
isTop _        = False

modify :: (MerkleTree a -> MerkleTree a) -> Loc (MerkleTree a) a -> Loc (MerkleTree a) a
modify f (m@(In (Pair (x, K h))), c) = (f m, c)

travel :: [Loc (MerkleTree a) a -> Loc (MerkleTree a) a] -> MerkleTree a -> MerkleTree a
travel dirs m = fst $ foldl (\x f -> f x) (m, Top) dirs

rehash :: Show a => MerkleTree a -> MerkleTree a
rehash (In (Pair (n@(Inr (Pair (Pair (I l, K x), I r))), K h))) = In (Pair (n, K h'))
  where
    h' = hash n
rehash _ = error "Cannot rehash a Leaf"

updateLoc :: Show a => (MerkleTree a -> MerkleTree a) -> Loc (MerkleTree a) a -> Loc (MerkleTree a) a
updateLoc f l = if isTop l' then l' else updateTree (up l')
  where
    l' = modify f l
    updateTree :: Show a => Loc (MerkleTree a) a -> Loc (MerkleTree a) a
    updateTree (x, Top) = (rehash x, Top)
    updateTree z@(x, c) = updateTree (up (rehash x, c))

applyDirs :: [Loc (MerkleTree a) a -> Loc (MerkleTree a) a]
          -> Loc (MerkleTree a) a
          -> Loc (MerkleTree a) a
applyDirs dirs mt = foldl (\x f -> f x) mt dirs

update :: Show a
       => (MerkleTree a -> MerkleTree a)
       -> [Loc (MerkleTree a) a -> Loc (MerkleTree a) a]
       -> MerkleTree a
       -> MerkleTree a
update f dirs mt = fromLoc $ updateLoc f loc'
  where
    loc' = foldl (\x f -> f x) (toLoc mt) dirs

insertTest :: MerkleTree Int -> MerkleTree Int
insertTest = update (const mt) [down]
  where
    mt = merkle (In (Inl (K 69)))
