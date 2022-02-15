{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics where

import           Control.Applicative        (liftA2)
import           Control.DeepSeq
import           Control.Monad.State
import qualified Data.Map                   as M
import           Debug.Trace                (trace)
import           GHC.Generics               (Generic, Generic1)
import           Generics.Data.Digest.CRC32

newtype Fix f = In { unFix :: f (Fix f) }

instance Eq (f (Fix f)) => Eq (Fix f) where
  f == g = unFix f == unFix g

instance Show (f (Fix f)) => Show (Fix f) where
  show = show . unFix

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg t = alg (fmap (cata alg) (unFix t))

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

{-
  GENERIC TREE
-}

debugHash :: Digest -> String
debugHash h = take 5 (show (getCRC32 h))

type MerkleFix f = Fix (f :*: K Digest)

merkle :: MerkelizeG f => Fix f -> MerkleFix f
merkle = In . merkleG . unFix

class (Functor f) => MerkelizeG f where
  merkleG :: (MerkelizeG g) => f (Fix g) -> (f :*: K Digest) (Fix (g :*: K Digest))

instance (Show a) => MerkelizeG (K a) where
  merkleG (K x) = Pair (K x, K h)
    where
      debug = trace ("Digest K: " ++ debugHash h)
      h = digestConcat [digest "K", digest x]

instance MerkelizeG I where
  merkleG (I x) = Pair (I prevX, K h)
    where
      debug = trace ("Digest I: " ++ debugHash h)
      prevX@(In (Pair (_, K ph))) = merkle x
      h = digestConcat [digest "I", ph]

instance (MerkelizeG f, MerkelizeG g) => MerkelizeG (f :+: g) where
  merkleG (Inl x) = Pair (Inl prevX, K h)
    where
      debug = trace ("Digest Inl: " ++ debugHash h)
      (Pair (prevX, K ph)) = merkleG x
      h = digestConcat [digest "Inl", ph]
  merkleG (Inr x) = Pair (Inr prevX, K h)
    where
      debug = trace ("Digest Inr: " ++ debugHash h)
      (Pair (prevX, K ph)) = merkleG x
      h = digestConcat [digest "Inr", ph]

instance (MerkelizeG f, MerkelizeG g) => MerkelizeG (f :*: g) where
  merkleG (Pair (x, y)) = Pair (Pair (prevX, prevY), K h)
    where
      debug = trace ("Digest Pair: " ++ debugHash h)
      (Pair (prevX, K phx)) = merkleG x
      (Pair (prevY, K phy)) = merkleG y
      h = digestConcat [digest "Pair", phx, phy]

cataMerkle :: Functor f => (f a -> a) -> M.Map String a -> Fix (f :*: K Digest) -> a
cataMerkle alg m (In (Pair (x, K h))) = case M.lookup (debugHash h) m of
  Just a  -> a
  Nothing -> alg (fmap (cataMerkle alg m) x)

cataMerkle2 :: (Functor f, Traversable f, Show a) => (f a -> a) -> Fix (f :*: K Digest) -> State (M.Map String a) a
cataMerkle2 alg (In (Pair (x, K h)))
  = do m <- get
       case M.lookup (debugHash h) m of
        Just a  -> trace ("LOOKUP: " ++ show a) return a
        Nothing -> trace ("CALCULATE: " ++ show m)
                 $ do y <- mapM (cataMerkle2 alg) x
                      let r = alg y
                      trace ("VALUE: " ++ show r) modify (M.insert (debugHash h) r) >> return r

cataMerkle3 :: (MerkelizeG f, Functor f, Traversable f, Show a) => (f a -> a) -> M.Map String a -> Fix f -> (a, M.Map String a)
cataMerkle3 alg m t = runState (cataMerkle2 alg (merkle t)) m

{-
  Nothing -> let resultaat = ... in modify (insert resultaat h) >> return resultaat

  modify :: (s -> s) -> State s ()

  cataMerkle2 :: ... -> State (M.Map String a) a

  sequence :: f (State Map a) -> State Map (f a)

  cataMerkle2 :: (Functor f, Traversable f) => (f a -> a) -> Fix (f :*: K Digest) -> State (M.Map String a) a
-}
