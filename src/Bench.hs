{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Bench (
#ifdef ACCELERATE_MEMORY_BENCH
  module Bench,
#else
  module Criterion,
  module Criterion.Main,
#endif
) where

#ifndef ACCELERATE_MEMORY_BENCH

import Criterion
import Criterion.Main

#else


import Data.Array.Accelerate.Array.Buffer ( memoryCounterReset, memoryCounterReport )
import Control.DeepSeq (deepseq, NFData)
import Control.Monad.Reader ( ReaderT, runReaderT, withReaderT, ask )
import Control.Monad.IO.Class ( liftIO )
import Data.List ( intercalate )


type Benchmark = ReaderT [String] IO ()

defaultMain :: [Benchmark] -> IO ()
defaultMain benchmarks = runReaderT (sequence_ benchmarks) []

env :: IO a -> (a -> Benchmark) -> Benchmark
env setup action = do
  !input <- liftIO setup
  action input

bgroup :: String -> [Benchmark] -> Benchmark
bgroup name = withReaderT (++ [name]) . sequence_

bench :: NFData a => String -> a -> Benchmark
bench name action = do
  bgroups <- ask
  liftIO $ putStrLn $ "benchmarking " ++ intercalate "/" (bgroups ++ [name]) ++ ":"
  liftIO memoryCounterReset
  deepseq action
    $ liftIO memoryCounterReport
  liftIO $ putStrLn ""

nf :: (a -> b) -> a -> b
nf = id

#endif
