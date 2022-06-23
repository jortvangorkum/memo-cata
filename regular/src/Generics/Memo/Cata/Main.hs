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

minHeight :: Int
minHeight = 5

cata :: Functor f => (f a -> a) -> AFix f b -> a
cata alg t = alg (fmap (cata alg) (unAFix t))

cataMerkleState :: (Functor f, Traversable f, C.Container c)
                => (f a -> a) -> AFix f MemoInfo -> State (c a) a
cataMerkleState alg (AFix x (MemoInfo d h))
  = do m <- get
       case C.lookup d m of
         Just a -> return a
         Nothing -> do y <- mapM (cataMerkleState alg) x
                       let r = alg y
                       if minHeight <= h
                         then modify (C.insert d r) >> return r
                       else return r
cataMerkle :: (Functor f, Traversable f, C.Container c)
           => (f a -> a) -> AFix f MemoInfo -> (a, c a)
cataMerkle alg t = runState (cataMerkleState alg t) C.empty

cataMerkleMap :: (Functor f, Traversable f, C.Container c)
              => (f a -> a) -> c a -> AFix f MemoInfo -> (a, c a)
cataMerkleMap alg m t = runState (cataMerkleState alg t) m

