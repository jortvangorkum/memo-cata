module Generics.Data.Dirs where

import           Control.DeepSeq

type Dirs = [Dir]
data Dir = Lft
         | Rght
         | Up
         | Bttm
         | Bttm'
         | Dwn
         | Dwn'

instance Show Dir where
  show Lft   = "Left"
  show Rght  = "Right"
  show Up    = "Up"
  show Bttm  = "Bottom"
  show Bttm' = "Bottom'"
  show Dwn   = "Down"
  show Dwn'  = "Down'"

instance NFData Dir where
  rnf x = x `seq` ()
