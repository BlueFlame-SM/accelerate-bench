{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Data.Array.Accelerate.Bench (
#ifdef ACCELERATE_MEMORY
  module Data.Array.Accelerate.Bench,
#else
  module Criterion,
  module Criterion.Main,
#endif
) where


#ifndef ACCELERATE_MEMORY

import Criterion
import Criterion.Main

#else

import Control.DeepSeq (deepseq, NFData)

type Benchmark = IO ()

defaultMain :: [Benchmark] -> IO ()
defaultMain = sequence_

env :: IO a -> (a -> Benchmark) -> Benchmark
env setup action = do
  !input <- setup
  action input

bgroup :: String -> [Benchmark] -> Benchmark
bgroup name benchmarks = do
  P.putStrLn $ "Benchmark group: " P.++ name
  sequence_ benchmarks

bench :: NFData a => String -> a -> Benchmark
bench name action = do
  P.putStrLn $ "Benchmark: " P.++ name
  memoryCounterReset
  deepseq action memoryCounterReport

nf :: (a -> b) -> a -> b
nf = P.id

#endif
