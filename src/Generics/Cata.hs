{-# LANGUAGE TypeOperators #-}
module Generics.Cata
  (
    cata,
    cataMerkle,
    cataMerkleMap,
  ) where

import           Control.Monad.State
import qualified Data.Map                   as M
import           Debug.Trace                (trace)
import           Generics
import           Generics.Data.Digest.CRC32 (Digest (getCRC32), debugHash)
import           Prelude                    hiding (lookup)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg t = alg (fmap (cata alg) (unFix t))

cataMerkleState :: (Functor f, Traversable f, Container c k, Show (c k a), Show a, Show k)
                => (f a -> a) -> Fix (f :*: K k) -> State (c k a) a
cataMerkleState alg (In (Pair (x, K h)))
  = do m <- get
       case lookup h m of
        Just a  -> trace ("LOOKUP: " ++ show a) return a
        Nothing -> trace ("CALCULATE: " ++ show m)
                 $ do y <- mapM (cataMerkleState alg) x
                      let r = alg y
                      trace ("VALUE: " ++ show r) modify (insert h r) >> return r

cataMerkle :: (Merkelize f, Functor f, Traversable f, Container c k, Show (c k a), Show a, Show k)
           => (f a -> a) -> Fix (f :*: K k) -> (a, c k a)
cataMerkle alg t = runState (cataMerkleState alg t) empty

cataMerkleMap :: (Merkelize f, Functor f, Traversable f, Container c k, Show (c k a), Show a, Show k)
              => (f a -> a) -> c k a -> Fix (f :*: K k) -> (a, c k a)
cataMerkleMap alg m t = runState (cataMerkleState alg t) m
