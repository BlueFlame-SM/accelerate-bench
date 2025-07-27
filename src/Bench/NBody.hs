{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Bench.NBody ( nbodyBench ) where
import Bench.NBody.NBody
import Data.Array.Accelerate (fromList, Z(..), pattern T3)
import Data.Array.Accelerate.Array.Buffer (memoryCounterReset, memoryCounterReport)
import Data.Array.Accelerate.LLVM.Native as CPU
-- import Data.Array.Accelerate.LLVM.PTX    as GPU

import Bench


nbodyBench :: Benchmark
nbodyBench = env (return $ CPU.runN nbody) $ \f ->
  bgroup "nbody"
    [ bench "n=1000, t=3600" $ nf (f (fromList Z [0.1]) (fromList Z [1000])) (fromList Z [3600])
    , bench "n=10000, t=600" $ nf (f (fromList Z [0.1]) (fromList Z [10000])) (fromList Z [600])
    , bench "n=100000, t=10" $ nf (f (fromList Z [0.1]) (fromList Z [100000])) (fromList Z [10])
    ]
