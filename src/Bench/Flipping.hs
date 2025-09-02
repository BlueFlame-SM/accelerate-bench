module Bench.Flipping (flippingBench) where


import qualified Prelude as P
import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe
import Data.Array.Accelerate.Data.Bits

import Data.Array.Accelerate.LLVM.Native

import Bench


flippingBench :: Benchmark
flippingBench = bgroup "flipping-switches"
  [ env setupFlipping $ \f -> bench "batch_size=12" $ nf (f switch_count (batch_size  9)) num_flips
  , env setupFlipping $ \f -> bench "batch_size=13" $ nf (f switch_count (batch_size 11)) num_flips
  , env setupFlipping $ \f -> bench "batch_size=14" $ nf (f switch_count (batch_size 13)) num_flips
  , env setupFlipping $ \f -> bench "batch_size=15" $ nf (f switch_count (batch_size 15)) num_flips
  ]
  where
    num_flips     = fromList Z [2 P.^ (32 :: Int)]
    switch_count  = fromList Z [2 P.^ (20 :: Int)]
    batch_size i  = fromList Z [2 P.^ (i  :: Int)]
    setupFlipping = P.return $ runN (flipping 8)




wanghash :: Exp Int -> Exp Int
wanghash seed0 = seed5
  where
    seed1 = (seed0 `xor` 61) `xor` (seed0 `shift` (-16))
    seed2 = 9 * seed1
    seed3 = seed2 `xor` (seed2 `shift` (-4))
    seed4 = 0x27d4eb2d * seed3
    seed5 = seed4 `xor` (seed4 `shift` (-15))


-- | Xorshift algorithm from George Marsaglia's paper
xorshift :: Exp Int -> Exp Int
xorshift seed0 = seed3
  where
    seed1 = seed0 `xor` (seed0 `shift` 13)
    seed2 = seed1 `xor` (seed1 `shift` (-17))
    seed3 = seed2 `xor` (seed2 `shift` 5)


integers :: Shape sh => Exp Int -> Acc (Array sh Int) -> Acc (Array sh Int, Array sh Int)
integers n seeds = T2 (map xorshift seeds) (map (`mod` n) seeds)


generator :: Shape sh => Exp sh -> Acc (Array sh Int)
generator sh = reshape sh $ generate (I1 (shapeSize sh)) (\(I1 i) -> wanghash i)


flipping :: Int -> Acc (Scalar Int) -> Acc (Scalar Int) -> Acc (Scalar Int) -> Acc (Vector Int)
flipping unrolls switch_count batch_size num_flips = res
  where
    zeros = fill (I1 $ the switch_count) 0
    ones  = fill (I1 $ the batch_size) 1
    seeds = generator (I1 $ the batch_size)

    T3 _ _ res = awhile f (unroll g unrolls) (T3 (unit 0) seeds zeros)

    f :: Acc (Scalar Int, Vector Int, Vector Int) -> Acc (Scalar Bool)
    f (T3 i _ _ ) = map (< the num_flips) i

    g :: Acc (Scalar Int, Vector Int, Vector Int)
      -> Acc (Scalar Int, Vector Int, Vector Int)
    g (T3 i seeds0 switches) = T3 (zipWith (+) i batch_size) seeds1 switches'
      where
        T2 seeds1 ixs = integers (the switch_count) seeds0
        switches' = permute xor switches (Just_ . I1 . (ixs !)) ones


unroll :: (a -> a) -> Int -> a -> a
unroll f n | n P.<= 1  = f
           | otherwise = f . unroll f (n-1)
