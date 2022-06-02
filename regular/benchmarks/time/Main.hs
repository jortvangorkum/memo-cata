module Main where

import           Benchmarks.SingleIter
import           Criterion.Main
import           MultiIter
import           Utils

-- MAIN
main :: IO ()
main = defaultMain
  [ -- singleIterBenches $ range (10 ^ 3) (10 ^ 6) 10
   multiIterBenches (ConfigIter { confNIters = 10, nodes = range (10 ^ 2) (10 ^ 5) 10, confScenario = Best })
  ]
