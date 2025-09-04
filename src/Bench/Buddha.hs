{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE BlockArguments #-}
module Bench.Buddha ( buddhaBench ) where

import qualified Prelude as P
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits (shift, xor)
import Data.Array.Accelerate.Data.Complex

import Data.Array.Accelerate.LLVM.Native (runN)

import Bench


buddhaBench :: Benchmark
buddhaBench = env (setupBuddha 2048) $ \f -> bgroup "buddhabrot"
  [ env (setupInput  100000) $ \m -> bench "n=100000"  $ nf f m
  , env (setupInput  500000) $ \m -> bench "n=500000"  $ nf f m
  , env (setupInput 1000000) $ \m -> bench "n=1000000" $ nf f m
  ]
  where
    setupBuddha numIters  = P.return $ runN (simpleBuddha 8 numIters)
    setupInput  numPoints = P.return $ fromList Z [numPoints]



wanghash :: Exp Word32 -> Exp Word32
wanghash seed0 = seed5
  where
    seed1 = (seed0 `xor` 61) `xor` (seed0 `shift` (-16))
    seed2 = 9 * seed1
    seed3 = seed2 `xor` (seed2 `shift` (-4))
    seed4 = 0x27d4eb2d * seed3
    seed5 = seed4 `xor` (seed4 `shift` (-15))


-- | Xorshift algorithm from George Marsaglia's paper
xorshift :: Exp Word32 -> Exp Word32
xorshift seed0 = seed3
  where
    seed1 = seed0 `xor` (seed0 `shift` 13)
    seed2 = seed1 `xor` (seed1 `shift` (-17))
    seed3 = seed2 `xor` (seed2 `shift` 5)


seed2float :: Exp Word32 -> Exp Float
seed2float seed = fromIntegral seed * (1.0 / 4294967296.0)


uniform :: Shape sh => Acc (Array sh Word32) -> Acc (Array sh Word32, Array sh Float)
uniform seeds = T2 (map xorshift seeds) (map seed2float seeds)


generator :: Shape sh => Exp sh -> Acc (Array sh Word32)
generator sh = reshape sh $ generate (I1 (shapeSize sh)) (\(I1 i) -> wanghash (fromIntegral i))


buddha :: Int -> Acc (Scalar Int) -> Acc (Scalar Int) -> Acc (Array DIM2 Int)
buddha num_unrolls num_points num_workers = asnd $ aiterate iters outer (T2 seeds zeros)
  where
    iters   = zipWith div num_points num_workers
    workers = the num_workers
    unrolls = constant num_unrolls

    px = 0.001875

    toindex :: Exp (Complex Float) -> Exp (Maybe DIM2)
    toindex z = if inbounds ix then Just_ ix else Nothing_
      where ix = toindex' z

    toindex' :: Exp (Complex Float) -> Exp DIM2
    toindex' z = I2 (floor ((imag z + 1.0125) / px)) (floor ((real z + 2.3) / px))

    inbounds :: Exp DIM2 -> Exp Bool
    inbounds (I2 y x) = (x >= 0) && (x < 1920) && (y >= 0) && (y < 1080)

    escapesin :: Exp (Complex Float) -> Exp (Bool, Int)
    escapesin c = T2 (magnitude z' > 2) i'
      where
        T2 i' z' = while
          (\(T2 i z) -> i < 2048 && (magnitude z <= 2))
          (\(T2 i z) -> T2 (i+1) (z*z + c))
          (T2 (0 :: Exp Int) c)

    zeros = fill (I2 1080 1920) 0
    seeds = generator (I1 workers)

    outer :: Acc (Vector Word32, Array DIM2 Int)
          -> Acc (Vector Word32, Array DIM2 Int)
    outer (T2 seeds0 hist) = T2 seeds2 (asnd $ aiterate (map ((`div` unrolls) . (+ (unrolls-1))) n) (unroll num_unrolls inner) (T2 cs' hist))
      where
        T2 seeds1 xs = uniform seeds0
        T2 seeds2 ys = uniform seeds1
        cs = zipWith (\x y -> mkPolar (2 * sqrt x) (2 * pi * y - pi)) xs ys
        (escapes, ns) = unzip (map escapesin cs)
        cs' = afst $ compact escapes cs
        n   = maximum (afst $ compact escapes ns)

        inner :: Acc (Vector (Complex Float), Array DIM2 Int)
              -> Acc (Vector (Complex Float), Array DIM2 Int)
        inner (T2 zs hist') = T2
          (zipWith (\c z -> z*z + c) cs' zs)
          (permute (+) hist' (toindex . (zs !)) (fill (shape cs') 1))


aiterate :: Arrays a => Acc (Scalar Int) -> (Acc a -> Acc a) -> Acc a -> Acc a
aiterate n f xs = asnd $ awhile (\(T2 i _) -> zipWith (<) i n) (\(T2 i x) -> T2 (map (+1) i) (f x)) (T2 (unit 0) xs)


unroll :: Int -> (a -> a) -> a -> a
unroll n f = if n P.<= 0 then P.id else unroll (n - 1) f . f



simpleBuddha :: Int -> Int -> Acc (Scalar Int) -> Acc (Array DIM2 Int)
simpleBuddha numUnroll numIters numPoints =
  let (T3 _ res _) = awhile loopCond (unroll numUnroll loopBody) start in res
  where
    height = 1080
    width  = 1920
    px     = 0.001875
    xmin   = -2.3
    ymin   = -1.0125
    xrange = px * fromIntegral width
    yrange = px * fromIntegral height

    seeds = generator (I1 $ the numPoints)
    T2 seeds' xs = uniform seeds
    T2 _      ys = uniform seeds'

    cs = afst $ filter escapes $ zipWith (\x y -> (x * xrange + xmin) ::+ (y * yrange + ymin)) xs ys

    escapes c = 2 < magnitude (snd $ while
      (\(T2 i z) -> i < constant numIters && (magnitude z <= 2))
      (\(T2 i z) -> T2 (i+1) (z*z + c))
      (T2 0 c))

    toindex z = let x = floor ((real z - xmin) / px)
                    y = floor ((imag z - ymin) / px)
                in if 0 <= x && x < width && 0 <= y && y < height then Just_ (I2 y x) else Nothing_

    start :: Acc (Scalar Int, Array DIM2 Int, Vector (Complex Float))
    start = T3 (unit 0) (fill (I2 height width) 0) cs

    loopCond :: Acc (Scalar Int, Array DIM2 Int, Vector (Complex Float)) -> Acc (Scalar Bool)
    loopCond (T3 i _ zs) = zipWith (&&) (map (< constant numIters) i) (any ((<= 2) . magnitude) zs)

    loopBody :: Acc (Scalar Int, Array DIM2 Int, Vector (Complex Float))
             -> Acc (Scalar Int, Array DIM2 Int, Vector (Complex Float))
    loopBody (T3 i hist zs) = do
      let hist' = permute (+) hist (toindex . (zs!)) (fill (shape zs) 1)
      let zs'   = zipWith (\c z -> z*z + c) cs zs
      T3 (map (+1) i) hist' zs'
