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
  , nNodes :: Int
  , nDirs  :: Int
  }

data EnvIter = EnvIter
  { changes :: Changes
  , curTree :: MerklePF (Tree Int)
  }

instance NFData EnvIter where
  rnf (EnvIter c t) = c `seq` t `seq` ()
