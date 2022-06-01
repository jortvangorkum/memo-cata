module Utils where

import           Control.DeepSeq
import qualified Data.Map             as M
import           GenericTree.Cata
import           GenericTree.Main
import           Generics.Memo.Main
import           Generics.Memo.Zipper

-- TYPES
type Changes = [Change]
data Change  = Change
  { newTree    :: MerklePF (Tree Int)
  , directions :: Dirs
  }

data ConfigIter = ConfigIter
  { nIters :: Int
  , nDirs  :: Int
  , nNodes :: Int
  }

data EnvIter = EnvIter
  { changes :: Changes
  , curTree :: MerklePF (Tree Int)
  }

instance NFData EnvIter where
  rnf (EnvIter c t) = c `seq` t `seq` ()

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
