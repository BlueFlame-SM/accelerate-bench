{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Array.Accelerate.Bench.Cocktail where

import qualified Prelude as P
import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe
import Data.Array.Accelerate.Data.Maybe ( fromMaybe )

import Data.Array.Accelerate.LLVM.Native

import Data.Array.Accelerate.Bench


cocktailBench :: Benchmark
cocktailBench = env setupInput $ \input -> bgroup "cocktail-sort"
  [ env (setupCocktail 1) $ \f -> bench "unroll=1" $ nf f input
  , env (setupCocktail 2) $ \f -> bench "unroll=2" $ nf f input
  , env (setupCocktail 4) $ \f -> bench "unroll=4" $ nf f input
  , env (setupCocktail 8) $ \f -> bench "unroll=8" $ nf f input
  ]
  where
    inputSize       = 2 P.^ 16
    setupInput      = P.return $ runN (generate (I1 inputSize) (\(I1 i) -> inputSize - i - 1))
    setupCocktail n = P.return $ runN (cocktailSort n)


-- | Cocktail sort implementation in Accelerate.
-- `n` is the number of times to perform the cocktail sort step in one iteration.
cocktailSort :: Ord a => Int -> Acc (Vector a) -> Acc (Vector a)
cocktailSort n = awhile notSorted (unroll cocktailStepLR n)


unroll :: (a -> a) -> Int -> a -> a
unroll f n | n P.<= 1  = f
           | otherwise = f . unroll f (n-1)


cocktailStepLR :: forall a. (Ord a) => Acc (Vector a) -> Acc (Vector a)
cocktailStepLR = cocktailStepR . cocktailStepL


cocktailStepRL :: forall a. (Ord a) => Acc (Vector a) -> Acc (Vector a)
cocktailStepRL = cocktailStepL . cocktailStepR


-- | Perform left-to-right sorting step, i.e. accumulate maximum value.
cocktailStepL :: forall a. (Ord a) => Acc (Vector a) -> Acc (Vector a)
cocktailStepL xs = map snd $ scanr1 (\(T2 lx lM) (T2 rx _) -> T2 lx (min lM rx)) $ scanl1 (\(T2 _ lM) (T2 rx rM) -> T2 rx (max lM rM)) $ zip xs xs


-- | Perform right-to-left sorting step, i.e. accumulate minimum value.
cocktailStepR :: forall a. (Ord a) => Acc (Vector a) -> Acc (Vector a)
cocktailStepR xs = map snd $ scanl1 (\(T2 lx _) (T2 rx rM) -> T2 rx (max rM lx)) $ scanr1 (\(T2 lx lM) (T2 _ rM) -> T2 lx (min lM rM)) $ zip xs xs


-- | Check if the array is not sorted.
notSorted :: (Ord a) => Acc (Vector a) -> Acc (Scalar Bool)
notSorted xs = or $ zipWith (>) (init xs) (tail xs)
