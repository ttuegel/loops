module Main where

import Criterion.Main

import Bench.Sum

main :: IO ()
main = defaultMain
    [ bgroup "sum"
        [ bgroup "foldl"
            [ bench "[]" $ nf bench_sum_foldl_list iters
            , bench "Vector" $ nf bench_sum_foldl_vector iters
            , bench "LoopPrim" $ nf bench_sum_foldl_LoopPrim iters
            , bench "Loop" $ nf bench_sum_foldl_loop iters
            ]
        , bgroup "foldr"
            [ bench "[]" $ nf bench_sum_foldr_list iters
            , bench "Vector" $ nf bench_sum_foldr_vector iters
            , bench "LoopPrim" $ nf bench_sum_foldr_LoopPrim iters
            , bench "Loop" $ nf bench_sum_foldr_loop iters
            ]
        ]
    ]
  where
    iters = 10000000
