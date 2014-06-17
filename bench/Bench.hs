module Main where

import Criterion.Main
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import Prelude hiding (foldr)

import Control.Monad.Loop.Unroll

main :: IO ()
main = defaultMain
    [ bgroup "left"
        {-
        - The foldl benchmarks are for pure performance. "Loop 1"
        - should perform on par with "Vector". The unrolled loops
        - should be faster, but subsequent performance gains are
        - expected to diminish. Unrolling a foldl' loop only prevents
        - a jump at the end of each iteration. The biggest gain will
        - come from improved branch prediction, but that doesn't scale
        - beyond a few unrollings.
        -}
        [ bench "sumLeftList" $ nf sumLeftList iters
        , bench "sumManual" $ nf sumManual iters
        , bench "sumLeftVector" $ nf sumLeftVector iters
        , bench "sumLeft1" $ nf sumLeft1 iters
        , bench "sumLeft2" $ nf sumLeft2 iters
        , bench "sumLeft4" $ nf sumLeft4 iters
        , bench "sumLeft8" $ nf sumLeft8 iters
        ]
    , bgroup "foldr"
        {-
        - The foldr benchmarks will show pretty poor performance
        - because foldr creates a new thunk for every iteration of the
        - loop. (This is common to foldr over all structures.) On the
        - other hand, the gains from unrolling will be much greater
        - than with foldl' because each subsequent unrolling prevents
        - another thunk from being allocated. (Thunk allocation is much
        - more expensive than just a jump.
        -}
        [ bench "[]" $ nf bench_sum_foldr_List iters
        , bench "Vector" $ nf bench_sum_foldr_Vector iters
        , bench "Loop 1" $ nf (bench_sum_foldr_LoopT unroll1) iters
        , bench "Loop 2" $ nf (bench_sum_foldr_LoopT unroll2) iters
        , bench "Loop 4" $ nf (bench_sum_foldr_LoopT unroll4) iters
        , bench "Loop 8" $ nf (bench_sum_foldr_LoopT unroll8) iters
        ]
    ]
  where
    iters :: Int
    iters = 10000000

sumLeft :: Unrolling n => Unroll n -> Int -> Int
{-# INLINE sumLeft #-}
sumLeft unroll = \n -> foldl' (+) 0 $ loop $ for unroll 0 (<= n) (+ 1)

sumLeft1, sumLeft2, sumLeft4, sumLeft8 :: Int -> Int
sumLeft1 = sumLeft unroll1
sumLeft2 = sumLeft unroll2
sumLeft4 = sumLeft unroll4
sumLeft8 = sumLeft unroll8

sumLeftList :: Int -> Int
sumLeftList n = foldl' (+) 0 [0..n]

sumLeftVector :: Int -> Int
sumLeftVector n = V.foldl' (+) 0 $ V.enumFromTo 0 n

bench_sum_foldr_List :: Int -> Int
bench_sum_foldr_List n = foldr (+) 0 [0..n]

bench_sum_foldr_Vector :: Int -> Int
bench_sum_foldr_Vector n =
    V.foldr (+) 0 $ V.enumFromTo 0 n

bench_sum_foldr_LoopT :: Unrolling n => Unroll n -> Int -> Int
{-# INLINE bench_sum_foldr_LoopT #-}
bench_sum_foldr_LoopT unroll = \n -> foldr (+) 0 $ loop $ for unroll 0 (<= n) (+ 1)

sumManual :: Int -> Int
sumManual n = go 0 0
  where
    go acc i | i <= n = go (acc + i) (i + 1)
             | otherwise = acc
