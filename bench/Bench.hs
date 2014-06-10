module Main where

import Criterion.Main

import Bench.Loop.Sum

main :: IO ()
main = defaultMain
    [ bgroup "Loop"
        [ bgroup "sum"
            [ bgroup "foldl"
                [ bench "[]" $ nf bench_sum_foldl_list iters
                , bench "Vector" $ nf bench_sum_foldl_vector iters
                , bench "LoopPrim" $ nf bench_sum_foldl_LoopPrim iters
                , bench "Loop" $ nf bench_sum_foldl_loop iters
                , bench "LoopT" $ nf bench_sum_foldl_loopT iters
                ]
            , bgroup "foldr"
                [ bench "[]" $ nf bench_sum_foldr_list iters
                , bench "Vector" $ nf bench_sum_foldr_vector iters
                , bench "LoopPrim" $ nf bench_sum_foldr_LoopPrim iters
                , bench "Loop" $ nf bench_sum_foldr_loop iters
                , bench "LoopT" $ nf bench_sum_foldr_loopT iters
                ]
            ]
        ]
    ]
  where
    iters = 10000000
