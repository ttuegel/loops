module Main where

import Criterion.Main
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import Prelude hiding (foldr)

import qualified Bench.Loop as Loop

main :: IO ()
main = defaultMain
    [ bgroup "External"
        [ bench "sumLeftList" $ nf sumLeftList iters
        , bench "sumManual" $ nf sumManual iters
        , bench "sumLeftVector" $ nf sumLeftVector iters
        , bench "sumRightList" $ nf sumRightList iters
        , bench "sumRightVector" $ nf sumRightVector iters
        ]
    , bgroup "Loop"
        [ bench "sumLeft" $ nf Loop.sumLeft iters
        , bench "sumRight" $ nf Loop.sumRight iters
        ]
    ]
  where
    iters :: Int
    iters = 10000000

sumLeftList :: Int -> Int
sumLeftList n = foldl' (+) 0 [0..n]

sumLeftVector :: Int -> Int
sumLeftVector n = V.foldl' (+) 0 $ V.enumFromTo 0 n

sumRightList :: Int -> Int
sumRightList n = foldr (+) 0 [0..n]

sumRightVector :: Int -> Int
sumRightVector n = V.foldr (+) 0 $ V.enumFromTo 0 n

sumManual :: Int -> Int
sumManual n = go 0 0
  where
    go acc i | i <= n = go (acc + i) (i + 1)
             | otherwise = acc
