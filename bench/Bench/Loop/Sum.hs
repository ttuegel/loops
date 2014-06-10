module Bench.Loop.Sum where

import Control.Monad.Loop
import Data.Foldable
import Data.Functor.Identity
import qualified Data.Vector.Unboxed as V
import Prelude hiding (foldr)

bench_sum_foldl_loop :: Int -> Int
bench_sum_foldl_loop n = foldl' (+) 0 $ (for 0 (<= n) (+ 1) :: Loop Int)

bench_sum_foldl_loopT :: Int -> Int
bench_sum_foldl_loopT n =
    foldl' (+) 0 $ (for 0 (<= n) (+ 1) :: LoopT Identity Int)

bench_sum_foldl_list :: Int -> Int
bench_sum_foldl_list n = foldl' (+) 0 [0..n]

bench_sum_foldl_vector :: Int -> Int
bench_sum_foldl_vector n = V.foldl' (+) 0 $ V.enumFromTo 0 n

bench_sum_foldl_LoopPrim :: Int -> Int
bench_sum_foldl_LoopPrim n = foldl' (+) 0 $ (For 0 (<= n) (+ 1) id :: LoopPrim Int)

bench_sum_foldr_loop :: Int -> Int
bench_sum_foldr_loop n = foldr (+) 0 $ (for 0 (<= n) (+ 1) :: Loop Int)

bench_sum_foldr_loopT :: Int -> Int
bench_sum_foldr_loopT n =
    foldr (+) 0 $ (for 0 (<= n) (+ 1) :: LoopT Identity Int)

bench_sum_foldr_list :: Int -> Int
bench_sum_foldr_list n = foldr (+) 0 [0..n]

bench_sum_foldr_vector :: Int -> Int
bench_sum_foldr_vector n = V.foldr (+) 0 $ V.enumFromTo 0 n

bench_sum_foldr_LoopPrim :: Int -> Int
bench_sum_foldr_LoopPrim n = foldr (+) 0 $ (For 0 (<= n) (+ 1) id :: LoopPrim Int)
