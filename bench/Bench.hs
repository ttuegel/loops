module Main where

import Criterion.Main
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import Prelude hiding (foldr)
import System.Random (randomIO)

import qualified Bench.ForEach as ForEach
import qualified Bench.Loop as Loop

main :: IO ()
main = do
    xsList <- mapM (const randomIO) $ replicate 10000000 ()
    xsVector <- return $! V.fromList xsList
    defaultMain
        [ bgroup "External"
            [ bench "sumLeftList" $ nf sumLeftList iters
            , bench "sumManual" $ nf sumManual iters
            , bench "sumLeftVector" $ nf sumLeftVector iters
            , bench "sumRightList" $ nf sumRightList iters
            , bench "sumRightVector" $ nf sumRightVector iters
            ]
        , bgroup "Loop"
            [ bench "sumLeft" $ nf Loop.sumLeft iters
            , bench "sumLeftReturn" $ nf Loop.sumLeftReturn iters
            , bench "sumRight" $ nf Loop.sumRight iters
            ]
        , bgroup "ForEach"
            [ bench "sumLeftList" $ nf ForEach.sumLeftList xsList
            , bench "sumLeftListForEach" $ nf ForEach.sumLeftListForEach xsList
            , bench "sumRightList" $ nf ForEach.sumRightList xsList
            , bench "sumRightListForEach" $ nf ForEach.sumRightListForEach xsList
            , bench "sumLeftVector" $ nf ForEach.sumLeftVector xsVector
            , bench "sumLeftVectorForEach" $ nf ForEach.sumLeftVectorForEach xsVector
            , bench "sumRightVector" $ nf ForEach.sumRightVector xsVector
            , bench "sumRightVectorForEach" $ nf ForEach.sumRightVectorForEach xsVector
            , bench "sumVectorST" $ nf ForEach.sumVectorST xsVector
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
