{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Generics.Data.Digest
  ( Digest(..)
  , digest
  , digestConcat
  ) where

import           Control.DeepSeq
import           Data.Bits               (shiftR, (.&.))
import           Data.ByteString         (ByteString, append, pack)
import           Data.ByteString.Builder (stringUtf8, toLazyByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Digest.CityHash
import           Data.Hashable
import           Data.LargeWord          (LargeKey (..), Word128)
import           Data.List               (foldl')
import           Data.Word               (Word64)
import           Unsafe.Coerce

newtype Digest = Digest { getDigest :: Word64 }
  deriving (Eq, Show, Ord)

instance NFData Digest where
  rnf (Digest bs) = rnf bs

instance Hashable Digest where
  hashWithSalt s x = hashWithSalt s . getDigest $ x

instance NFData Word128 where
  rnf (LargeKey w1 w2) = rnf w1 `seq` rnf w2

instance Hashable Word128 where
  hashWithSalt s (LargeKey w1 w2) = s + (hash w1) + (hash w2)

toByteString :: Show a => a -> ByteString
toByteString = toStrict . toLazyByteString . stringUtf8 . show

digest :: Show a => a -> Digest
digest = Digest . cityHash64 . toByteString

combineDigest :: Digest -> Digest -> Digest
combineDigest d1 d2 = Digest $ cityHash64WithSeed (w64ToBs (getDigest d1)) (getDigest d2)

digestConcat :: [Digest] -> Digest
digestConcat []     = error "No Empty List for digestConcat"
digestConcat [x]    = x
digestConcat (x:xs) = foldl' (flip combineDigest) x xs

-- Helper functions

w128ToBs :: Word128 -> ByteString
w128ToBs (LargeKey first64 next64) =
    w64ToBs first64 `append` w64ToBs next64

w64ToBs :: Word64 -> ByteString
w64ToBs w64 =
    pack
    [ unsafeCoerce (w64 `shiftR` 56 .&. 255)
    , unsafeCoerce (w64 `shiftR` 48 .&. 255)
    , unsafeCoerce (w64 `shiftR` 40 .&. 255)
    , unsafeCoerce (w64 `shiftR` 32 .&. 255)
    , unsafeCoerce (w64 `shiftR` 24 .&. 255)
    , unsafeCoerce (w64 `shiftR` 16 .&. 255)
    , unsafeCoerce (w64 `shiftR` 8 .&. 255)
    , unsafeCoerce (w64 .&. 255)
    ]
