module Main (main) where

import Bench ( defaultMain )
import Bench.Cocktail ( cocktailBench )
import Bench.MG ( mgBench )
import Bench.NBody ( nbodyBench )
import Bench.Quickhull ( quickhullBench )

main :: IO ()
main = defaultMain
  [ cocktailBench
  , mgBench
  , nbodyBench
  , quickhullBench
  ]
