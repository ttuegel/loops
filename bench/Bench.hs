module Main where

import Criterion.Main
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import Prelude hiding (foldr)

import Control.Monad.Loop.Unroll

main :: IO ()
main = defaultMain
    [ bgroup "sum"
        [ bgroup "foldl"
            {-
            - The foldl benchmarks are for pure performance. "Loop 1"
            - should perform on par with "Vector". The unrolled loops
            - should be faster, but subsequent performance gains are
            - expected to diminish. Unrolling a foldl' loop only prevents
            - a jump at the end of each iteration. The biggest gain will
            - come from improved branch prediction, but that doesn't scale
            - beyond a few unrollings.
            -}
            [ bench "[]" $ nf bench_sum_foldl_List iters
            , bench "Vector" $ nf bench_sum_foldl_Vector iters
            , bench "Loop 1" $ nf (bench_sum_foldl_LoopT unroll1) iters
            , bench "Loop 2" $ nf (bench_sum_foldl_LoopT unroll2) iters
            , bench "Loop 4" $ nf (bench_sum_foldl_LoopT unroll4) iters
            , bench "Loop 8" $ nf (bench_sum_foldl_LoopT unroll8) iters
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
    ]
  where
    iters :: Int
    iters = 10000000

bench_sum_foldl_LoopT :: Unrolling n => Unroll n -> Int -> Int
{-# INLINE bench_sum_foldl_LoopT #-}
bench_sum_foldl_LoopT unroll = \n -> foldl' (+) 0 $ loop $ for unroll 0 (<= n) (+ 1)

bench_sum_foldl_List :: Int -> Int
bench_sum_foldl_List n = foldl' (+) 0 [0..n]

bench_sum_foldl_Vector :: Int -> Int
bench_sum_foldl_Vector n = V.foldl' (+) 0 $ V.enumFromTo 0 n

bench_sum_foldr_List :: Int -> Int
bench_sum_foldr_List n = foldr (+) 0 [0..n]

bench_sum_foldr_Vector :: Int -> Int
bench_sum_foldr_Vector n =
    V.foldr (+) 0 $ V.enumFromTo 0 n

bench_sum_foldr_LoopT :: Unrolling n => Unroll n -> Int -> Int
{-# INLINE bench_sum_foldr_LoopT #-}
bench_sum_foldr_LoopT unroll = \n -> foldr (+) 0 $ loop $ for unroll 0 (<= n) (+ 1)
