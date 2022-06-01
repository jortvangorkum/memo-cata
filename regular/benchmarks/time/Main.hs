module Main where

import           Benchmarks.MultiIter
import           Benchmarks.SingleIter
import           Criterion.Main
import           Utils

-- MAIN
main :: IO ()
main = defaultMain
  [ singleIterBenches $ range (10 ^ 3) (10 ^ 6) 10
  -- , multiIterBenches (ConfigIter { nNodes = 8, nIters = 10, nDirs = 10 })
  ]
