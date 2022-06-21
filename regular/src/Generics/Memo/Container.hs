{-# LANGUAGE FlexibleInstances #-}

module Generics.Memo.Container where

import qualified Data.HashMap.Strict  as H
import qualified Data.Map             as M
import           Generics.Data.Digest
import           Generics.Memo.Main

class Container c where
  empty  :: c a
  insert :: MemoInfo -> a -> c a -> c a
  lookup :: MemoInfo -> c a -> Maybe a

instance Container (M.Map Digest) where
  empty  = M.empty
  insert = M.insert . getDigest
  lookup = M.lookup . getDigest

instance Container (H.HashMap Digest) where
  empty  = H.empty
  insert = H.insert . getDigest
  lookup = H.lookup . getDigest
