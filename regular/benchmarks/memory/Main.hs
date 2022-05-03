module Main where

import           Criterion.Main
import           MultiIter
import           SingleIter
import           Utils

-- MAIN
main :: IO ()
main = defaultMain
  [ singleIterBenches 5
  , multiIterBenches (ConfigIter { nNodes = 5, nIters = 10, nDirs = 10 })
  ]
