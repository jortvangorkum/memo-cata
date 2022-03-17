{-# LANGUAGE TypeOperators #-}
module Generics.Cata
  (
    cata,
    cataMerkle,
    cataMerkleMap,
  ) where

import           Control.Monad.State
import qualified Data.Map                   as M
import           Generics.Data.Digest.CRC32 (Digest (getCRC32))
import           Generics.Main
import           Prelude                    hiding (lookup)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg t = alg (fmap (cata alg) (unFix t))

cataMerkleState :: (Functor f, Traversable f, Container c, Show (c a), Show a)
                => (f a -> a) -> Fix (f :*: K Digest) -> State (c a) a
cataMerkleState alg (In (Pair (x, K h)))
  = do m <- get
       case lookup h m of
        Just a  -> return a
        Nothing -> do y <- mapM (cataMerkleState alg) x
                      let r = alg y
                      modify (insert h r) >> return r

cataMerkle :: (Traversable f, Container c, Show (c a), Show a)
           => (f a -> a) -> Fix (f :*: K Digest) -> (a, c a)
cataMerkle alg t = runState (cataMerkleState alg t) empty

cataMerkleMap :: (Merkelize f, Functor f, Traversable f, Container c, Show (c a), Show a)
              => (f a -> a) -> c a -> Fix (f :*: K Digest) -> (a, c a)
cataMerkleMap alg m t = runState (cataMerkleState alg t) m
