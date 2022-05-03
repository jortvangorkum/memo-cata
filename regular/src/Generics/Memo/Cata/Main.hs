{-# LANGUAGE TypeOperators #-}

module Generics.Memo.Cata.Main
  ( cata
  , cataMerkle
  , cataMerkleMap
  ) where

import           Control.Monad.State
import qualified Data.Map                   as M
import           Generics.Data.Digest.CRC32
import           Generics.Regular.Base

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg t = alg (fmap (cata alg) (out t))

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

cataMerkleMap :: (Functor f, Traversable f)
              => (f a -> a) -> M.Map Digest a -> Fix (f :*: K Digest) -> (a, M.Map Digest a)
cataMerkleMap alg m t = runState (cataMerkleState alg t) m
