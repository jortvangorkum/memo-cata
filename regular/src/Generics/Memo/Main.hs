{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Memo.Main where

import           Control.DeepSeq
import           Data.Functor.Classes
import           Data.Tuple.Extra      ((&&&))
import           Generics.Data.Digest
import           Generics.Regular.Base

data MemoInfo = MemoInfo { getDigest :: Digest, getHeight :: Int }
  deriving (Show, Eq)

class Merkelize f where
  merkelize :: f (Fix (g :*: K MemoInfo)) -> MemoInfo

instance (Show a) => Merkelize (K a) where
  merkelize (K x) = MemoInfo d h
    where
      d = digestConcat [digest "K", digest x]
      h = 1

instance Merkelize I where
  merkelize (I (In (_ :*: K (MemoInfo pd ph)))) = MemoInfo d h
    where
      d = digestConcat [digest "I", pd]
      h = 1 + ph

instance (Merkelize f, Merkelize g) => Merkelize (f :+: g) where
  merkelize (L x) = MemoInfo d ph
    where
      (MemoInfo pd ph) = merkelize x
      d = digestConcat [digest "L", pd]
  merkelize (R x) = MemoInfo d ph
    where
      (MemoInfo pd ph) = merkelize x
      d = digestConcat [digest "R", pd]

instance (Merkelize f, Merkelize g) => Merkelize (f :*: g) where
  merkelize (x :*: y) = MemoInfo d h
    where
      (MemoInfo pd1 ph1) = merkelize x
      (MemoInfo pd2 ph2) = merkelize y
      d = digestConcat [digest "P", pd1, pd2]
      h = max ph1 ph2

instance (Merkelize f) => Merkelize (C c f) where
  merkelize (C x) = MemoInfo d ph
    where
      (MemoInfo pd ph) = merkelize x
      d = digestConcat [digest "C", pd]

instance Merkelize U where
  merkelize _ = MemoInfo (digest "U") 1

type MerklePF f = Merkle (PF f)
type Merkle f = Fix (f :*: K MemoInfo)
type instance PF (Merkle f) = f :*: K MemoInfo
instance Regular (Merkle f) where
  from = out
  to   = In

merkleG :: Merkelize f => f (Fix (g :*: K MemoInfo)) -> (f :*: K MemoInfo) (Fix (g :*: K MemoInfo))
merkleG f = f :*: K (merkelize f)

merkle :: (Regular a, Merkelize (PF a), Functor (PF a)) => a -> Fix (PF a :*: K MemoInfo)
merkle = In . merkleG . fmap merkle . from


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

-- Generic Equality
instance Eq (f (Fix f)) => Eq (Fix f) where
  f == g = out f == out g

instance Eq a => Eq1 (K a) where
  eq1 (K x) (K y) = x == y

instance (Eq a, Eq r) => Eq (K a r) where
  (==) = eq1

instance Eq1 I where
  eq1 = (==)

instance Eq r => Eq (I r) where
  I x == I y = x == y

instance (Eq1 f, Eq1 g) => Eq1 (f :+: g) where
  eq1 (L x) (L y) = eq1 x y
  eq1 (R x) (R y) = eq1 x y
  eq1 _     _     = False

instance (Eq1 f, Eq1 g, Eq r) => Eq ((f :+: g) r) where
  (==) = eq1

instance (Eq1 f, Eq1 g) => Eq1 (f :*: g) where
  eq1 (x1 :*: y1) (x2 :*: y2) = eq1 x1 x2 && eq1 y1 y2

instance (Eq1 f, Eq1 g, Eq r) => Eq ((f :*: g) r) where
  (==) = eq1

instance (Eq1 f) => Eq1 (C c f) where
  eq1 (C x) (C y) = eq1 x y

instance (Eq1 f, Eq r) => Eq (C c f r) where
  (==) = eq1

-- Generic Show
instance Show (f (Fix f)) => Show (Fix f) where
  show = show . out

instance Show a => Show1 (K a) where
  showsPrec1 n (K x) = showsPrec n x

instance (Show a, Show r) => Show (K a r) where
  showsPrec = showsPrec1

instance Show1 I where
  showsPrec1 = showsPrec

instance Show r => Show (I r) where
  showsPrec n (I r) = showsPrec n r

instance (Show1 f, Show1 g) => Show1 (f :+: g) where
  showsPrec1 n (L x) = showsPrec1 n x
  showsPrec1 n (R x) = showsPrec1 n x

instance (Show1 f, Show1 g, Show r) => Show ((f :+: g) r) where
  showsPrec = showsPrec1

instance (Show1 f, Show1 g) => Show1 (f :*: g) where
  showsPrec1 n (x :*: y) = showsPrec1 n x . showString " " . showsPrec1 n y

instance (Show1 f, Show1 g, Show r) => Show ((f :*: g) r) where
  showsPrec = showsPrec1

showBraces :: Bool -> ShowS -> ShowS
showBraces b p = if b then showChar '{' . p . showChar '}' else p

instance (Constructor c, Show1 f) => Show1 (C c f) where
  showsPrec1 n cx@(C x) = case fixity of
    Prefix -> showParen True (showString (conName cx) . showChar ' ' . showBraces isRecord (showsPrec1 n x))
    Infix _ _ -> showParen True
                    (showChar '(' . showString (conName cx)
                     . showChar ')' . showChar ' '
                     . showBraces isRecord (showsPrec1 n x))
    where isRecord = conIsRecord cx
          fixity   = conFixity cx

instance (Constructor c, Show1 f, Show r) => Show (C c f r) where
  showsPrec = showsPrec1

-- Generic NFData
-- https://hackage.haskell.org/package/deepseq-1.4.6.1/docs/src/Control.DeepSeq.html#line-535

-- # COPIED FROM NEWER VERSIONS OF DEEPSEQ
class NFData1 f where
  liftRnf :: (a -> ()) -> f a -> ()

rnf1 :: (NFData1 f, NFData a) => f a -> ()
rnf1 = liftRnf rnf

rwhnf :: a -> ()
rwhnf = (`seq` ())
-- #

instance NFData (f (Fix f)) => NFData (Fix f) where
  rnf (In x) = rnf x

instance NFData MemoInfo where
  rnf (MemoInfo d h) = rnf d `seq` rnf h

instance NFData r => NFData (I r) where
  rnf = rnf1

instance NFData1 I where
  liftRnf f (I x) = f x

instance (NFData a, NFData r) => NFData (K a r) where
  rnf = rnf1

instance NFData a => NFData1 (K a) where
  liftRnf _ (K x) = rwhnf x

instance (NFData1 f, NFData1 g) => NFData1 (f :+: g) where
  liftRnf f (L x) = liftRnf f x
  liftRnf f (R x) = liftRnf f x

instance (NFData1 f, NFData1 g, NFData r) => NFData ((f :+: g) r) where
  rnf = rnf1

instance (NFData1 f, NFData1 g) => NFData1 (f :*: g) where
  liftRnf f (x :*: y) = liftRnf f x `seq` liftRnf f y

instance (NFData1 f, NFData1 g, NFData r) => NFData ((f :*: g) r) where
  rnf = rnf1

instance NFData1 f => NFData1 (C c f) where
  liftRnf f (C x) = liftRnf f x

instance (NFData1 f, NFData r) => NFData (C c f r) where
  rnf = rnf1
