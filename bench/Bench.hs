module Main where

import Criterion.Main

import Bench.Sum

main :: IO ()
main = defaultMain
    [ bgroup "sum"
        [ bgroup "foldl"
            [ bench "[]" $ nf bench_sum_foldl_List iters
            , bench "Vector" $ nf bench_sum_foldl_Vector iters
            , bench "LoopPrim" $ nf bench_sum_foldl_LoopPrim iters
            , bench "LoopT Identity" $ nf bench_sum_foldl_LoopT_Identity iters
            , bench "Loop" $ nf bench_sum_foldl_Loop iters
            ]
        , bgroup "foldr"
            [ bench "[]" $ nf bench_sum_foldr_List iters
            , bench "Vector" $ nf bench_sum_foldr_Vector iters
            , bench "LoopPrim" $ nf bench_sum_foldr_LoopPrim iters
            , bench "LoopT Identity" $ nf bench_sum_foldr_LoopT_Identity iters
            , bench "Loop" $ nf bench_sum_foldr_Loop iters
            ]
        ]
    ]
  where
    iters = 10000000
