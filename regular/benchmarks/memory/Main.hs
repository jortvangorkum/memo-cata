module Main where

import           MultiIter
import           SingleIter

-- MAIN
main :: IO ()
main = defaultMain
  [ singleIterBenches 10
  , multiIterBenches 10
  ]
