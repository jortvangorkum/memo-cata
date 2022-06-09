{-# LANGUAGE FlexibleInstances #-}

module Generics.Memo.Container where

import           Data.ByteString            (ByteString)
import qualified Data.HashMap.Strict        as H
import qualified Data.Map                   as M
import           Generics.Data.Digest.CRC32

class Container c where
  empty  :: c a
  insert :: Digest -> a -> c a -> c a
  lookup :: Digest -> c a -> Maybe a

instance Container (M.Map ByteString) where
  empty  = M.empty
  insert = M.insert . getByteString
  lookup = M.lookup . getByteString

instance Container (H.HashMap ByteString) where
  empty  = H.empty
  insert = H.insert . getByteString
  lookup = H.lookup . getByteString
