{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Data.Digest.CRC32 where

import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (stringUtf8, toLazyByteString,
                                          word32LE)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Digest.CRC32
import           Data.List               (foldl', splitAt)
import           Data.Maybe              (fromJust, fromMaybe)
import           Data.Word               (Word32)

-- TYPES / INSTANCES

data Digest = Digest { getCRC32 :: Word32, getByteString :: ByteString }
  deriving (Eq)

instance Show Digest where
  show = show . getByteString

instance Ord Digest where
  compare x y = compare (getByteString x) (getByteString y)

debugHash :: Digest -> String
debugHash = take 5 . show

hashStr :: String -> Digest
hashStr s = Digest w32 bs
  where
    bs  = toStrict $ toLazyByteString $ stringUtf8 s
    w32 = crc32 bs

-- | Concatenates digests together and hashes the result.
digestConcat :: [Digest] -> Digest
digestConcat []     = error "No Empty List for digestConcat"
digestConcat [x]    = x
digestConcat (x:xs) = foldl' (flip combineCRC32) x xs

-- Digestible

class Digestible a where
  digest :: a -> Digest

instance Show a => Digestible a where
  digest = hashStr . show

-- CRC32

instance CRC32 Digest where
  crc32Update w = crc32Update w . getByteString

combineCRC32 :: Digest -> Digest -> Digest
combineCRC32 x y = dig'
  where
    w32' = crc32Update (getCRC32 x) (getByteString y)
    bs'  = toStrict $ toLazyByteString $ word32LE w32'
    dig' = Digest w32' bs'

