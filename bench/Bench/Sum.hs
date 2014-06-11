module Bench.Sum where

import qualified Control.Loop as Loop
import qualified Control.Monad.Loop as LoopT
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import Prelude hiding (foldr)

bench_sum_foldl_Loop :: Int -> Int
bench_sum_foldl_Loop n =
    foldl' (+) 0 (Loop.for 0 (<= n) (+ 1) :: Loop.Loop Int)

bench_sum_foldl_LoopT :: Int -> Int
bench_sum_foldl_LoopT n =
    foldl' (+) 0 (LoopT.for 0 (<= n) (+ 1) :: LoopT.Loop Int)

bench_sum_foldl_List :: Int -> Int
bench_sum_foldl_List n = foldl' (+) 0 [0..n]

bench_sum_foldl_Vector :: Int -> Int
bench_sum_foldl_Vector n = V.foldl' (+) 0 $ V.enumFromTo 0 n

bench_sum_foldr_Loop :: Int -> Int
bench_sum_foldr_Loop n =
    foldr (+) 0 (Loop.for 0 (<= n) (+ 1) :: Loop.Loop Int)

bench_sum_foldr_List :: Int -> Int
bench_sum_foldr_List n = foldr (+) 0 [0..n]

bench_sum_foldr_Vector :: Int -> Int
bench_sum_foldr_Vector n =
    V.foldr (+) 0 $ V.enumFromTo 0 n

bench_sum_foldr_LoopT :: Int -> Int
bench_sum_foldr_LoopT n =
    foldr (+) 0 (LoopT.for 0 (<= n) (+ 1) :: LoopT.Loop Int)
