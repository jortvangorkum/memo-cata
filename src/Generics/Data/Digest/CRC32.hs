
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Data.Digest.CRC32 where

import           Data.ByteString            (ByteString, unpack)
import           Data.ByteString.Builder    (stringUtf8, word32LE)
import           Data.ByteString.Conversion (fromByteString, toByteString)
import           Data.Digest.CRC32
import           Data.List                  (foldl', splitAt)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Word                  (Word32, Word64, Word8)
import Data.Text (pack, encodeUtf8)

-- TYPES / INSTANCES

newtype Digest = Digest { getByteString :: ByteString }
  deriving (Eq , Show)

instance Ord Digest where
  compare x y = compare (getByteString x) (getByteString y)

debugHash :: Digest -> String
debugHash h = take 5 (show (getByteString h))

hashStr :: String -> Digest
hashStr = Digest . toByteString . word32LE . crc32 . toByteString . stringUtf8
  where
    str2bystr :: String -> ByteString
    str2bystr = encodeUtf8 . pack

convertDigestToWord32 :: Digest -> Word32
convertDigestToWord32 x = undefined
  where
    w32 = fromIntegral w8s
    w8s = fromByteString $ getByteString x


-- | Concatenates digests together and hashes the result.
digestConcat :: [Digest] -> Digest
digestConcat []     = error "No Empty List for digestConcat"
digestConcat (x:xs) = foldl' (flip combineCRC32) x xs

-- Digestible
class Digestible a where
  digest :: [a] -> Digest

instance Show a => Digestible a where
  digest = digestConcat . map (hashStr . show)

-- CRC32

instance CRC32 Digest where
  crc32Update w = crc32Update w . getByteString

combineCRC32 :: Digest -> Digest -> Digest
combineCRC32 dig x = dig'
  where
    h :: Word32
    h = convertDigestToWord32 dig
    dig' = Digest $ toByteString $ crc32Update h (getByteString x)

