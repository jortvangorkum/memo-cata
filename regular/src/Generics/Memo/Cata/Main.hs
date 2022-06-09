{-# LANGUAGE TypeOperators #-}

module Generics.Memo.Cata.Main
  ( cata
  , cataMerkle
  , cataMerkleMap
  ) where

import           Control.Monad.State
import           Generics.Data.Digest
import qualified Generics.Memo.Container as C
import           Generics.Memo.Main
import           Generics.Regular.Base

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg t = alg (fmap (cata alg) (out t))

cataMerkleState :: (Functor f, Traversable f, C.Container c)
                => (f a -> a) -> Fix (f :*: K Digest) -> State (c a) a
cataMerkleState alg (In (x :*: K h))
  = do m <- get
       case C.lookup h m of
         Just a -> return a
         Nothing -> do y <- mapM (cataMerkleState alg) x
                       let r = alg y
                       modify (C.insert h r) >> return r

cataMerkle :: (Functor f, Traversable f, C.Container c)
           => (f a -> a) -> Fix (f :*: K Digest) -> (a, c a)
cataMerkle alg t = runState (cataMerkleState alg t) C.empty

cataMerkleMap :: (Functor f, Traversable f, C.Container c)
              => (f a -> a) -> c a -> Fix (f :*: K Digest) -> (a, c a)
cataMerkleMap alg m t = runState (cataMerkleState alg t) m

