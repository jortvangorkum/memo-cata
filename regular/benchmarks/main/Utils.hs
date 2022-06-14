module Utils where

import           Control.DeepSeq
import           Data.ByteString      (ByteString)
import qualified Data.HashMap.Strict  as H
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Data.Digest
import           Generics.Memo.Main
import           Generics.Memo.Zipper

-- TYPES
type Changes = [Change]
data Change  = Change
  { directions :: Dirs
  , newTree    :: Tree Int
  }

data Case = Worst | Average | Best deriving (Enum, Show)
data ConfigIter = ConfigIter
  { confNIters   :: Int
  , confScenario :: Case
  , nodes        :: [Int]
  }

data ConfigEnv = ConfigEnv
  { nIters   :: Int
  , scenario :: Case
  , nNodes   :: Int
  }
data EnvIter = EnvIter
  { curTree      :: Tree Int
  , curContainer :: H.HashMap Digest Int
  , changes      :: Changes
  }

instance NFData EnvIter where
  rnf (EnvIter t c cs) = cs `seq` c `seq` t `seq` ()

log10 :: Floating a => a -> a
log10 = logBase 10
range :: Int -> Int -> Int -> [Int]
range l u n =  [round (10 ** (log10 l' + (i * s))) | i <- [0.0, 1.0 .. (n' - 1.0)]]
  where
    l', u', n', s :: Float
    l' = fromIntegral l
    u' = fromIntegral u
    n' = fromIntegral n
    s  = (log10 u' - log10 l') / (n' - 1)
