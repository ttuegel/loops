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
            [ bench "[]" $ nf bench_sum_foldl_List iters
            , bench "Vector" $ nf bench_sum_foldl_Vector iters
            , bench "Loop 1" $ nf (bench_sum_foldl_LoopT unroll1) iters
            , bench "Loop 2" $ nf (bench_sum_foldl_LoopT unroll2) iters
            , bench "Loop 4" $ nf (bench_sum_foldl_LoopT unroll4) iters
            , bench "Loop 8" $ nf (bench_sum_foldl_LoopT unroll8) iters
            ]
        , bgroup "foldr"
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
