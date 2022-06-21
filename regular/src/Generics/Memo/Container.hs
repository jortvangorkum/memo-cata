{-# LANGUAGE FlexibleInstances #-}

module Generics.Memo.Container where

import qualified Data.HashMap.Strict  as H
import qualified Data.Map             as M
import           Generics.Data.Digest
import           Generics.Memo.Main

class Container c where
  empty  :: c a
  insert :: Digest -> a -> c a -> c a
  lookup :: Digest -> c a -> Maybe a

instance Container (M.Map Digest) where
  empty  = M.empty
  insert = M.insert
  lookup = M.lookup

instance Container (H.HashMap Digest) where
  empty  = H.empty
  insert = H.insert
  lookup = H.lookup
