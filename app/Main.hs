module Main (main) where

import Bench ( defaultMain )
import Bench.Buddha ( buddhaBench )
import Bench.Cocktail ( cocktailBench )
import Bench.MG ( mgBench )
import Bench.NBody ( nbodyBench )
import Bench.Quickhull ( quickhullBench )

main :: IO ()
main = defaultMain [buddhaBench]
  -- [ buddhaBench
  -- , cocktailBench
  -- , mgBench
  -- , nbodyBench
  -- , quickhullBench
  -- ]
