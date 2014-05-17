module Main where

import Criterion.Main

import Bench.Loop.Sum

main :: IO ()
main = defaultMain
    [ bgroup "Loop"
        [ bgroup "Sum"
            [ bgroup "foldl"
                [ bench "list" $ nf bench_sum_foldl_list iters
                , bench "vector" $ nf bench_sum_foldl_vector iters
                , bench "loop" $ nf bench_sum_foldl_loop iters
                , bench "LoopPrim" $ nf bench_sum_foldl_LoopPrim iters
                ]
            , bgroup "foldr"
                [ bench "list" $ nf bench_sum_foldr_list iters
                , bench "vector" $ nf bench_sum_foldr_vector iters
                , bench "loop" $ nf bench_sum_foldr_loop iters
                ]
            ]
        ]
    ]
  where
    iters = 10000000
