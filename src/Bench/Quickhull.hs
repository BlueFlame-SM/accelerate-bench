{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
module Bench.Quickhull ( quickhullBench ) where
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- import qualified Data.Array.Accelerate.LLVM.PTX    as GPU

import Bench.Quickhull.Quickhull
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

import Data.Int
import Foreign.Storable
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, withForeignPtr)

import Bench


quickhullBench :: Benchmark
quickhullBench = bgroup "quickhull"
  [ env (load "25M_rectangle") quickhullBench'
  , env (load "25M_circle")    quickhullBench'
  , env (load "25M_quadratic") quickhullBench'
  ]


quickhullBench' :: Input -> Benchmark
quickhullBench' (name, input) = bgroup name
  [ env (return $ CPU.runN quickhull1) $ \f -> bench "flat"  $ nf f input
  , env (return $ CPU.runN quickhull2) $ \f -> bench "split" $ nf f input
  , env (return $ recursive 2)         $ \f -> bench "rec-2" $ nf f input
  , env (return $ recursive 5)         $ \f -> bench "rec-5" $ nf f input
  ]
  where
    recursive = quickhullRecursiveThenFlatten CPU.runN


type Input = (String, A.Vector Point)


load :: String -> IO Input
load name = do
  putStrLn $ "Loading " ++ name
  content <- B.readFile $ "input/" ++ name ++ ".dat"
  let (fptrw8, nw8) = BI.toForeignPtr0 content
  res <- withForeignPtr (castForeignPtr fptrw8 :: ForeignPtr Int32) $ \ptr ->
    A.fromFunctionM (A.Z A.:. (nw8 `quot` 8))
      (\(A.Z A.:. ix) -> do
        x <- peekElemOff ptr (2 * ix)
        y <- peekElemOff ptr (2 * ix + 1)
        return (fromIntegral x, fromIntegral y))
  return (name, res)


-- testInput :: (String, A.Vector Point -> A.Vector Point) -> Input -> IO ()
-- testInput (backend, f) (inputName, inputData) = do
--   putStrLn $ backend ++ "/" ++ inputName
--   putStrLn $ take 80 $ show $ f inputData
--   putStrLn ""
