module Bench.ForEach where

import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import Data.Foldable
import Data.STRef
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Prelude hiding (foldr)

import Control.Monad.Loop

sumLeftList :: [Int] -> Int
sumLeftList = foldl' (+) 0

sumLeftListForEach :: [Int] -> Int
sumLeftListForEach xs = foldl' (+) 0 $ loop $ forEach xs

sumRightList :: [Int] -> Int
sumRightList = foldr (+) 0

sumRightListForEach :: [Int] -> Int
sumRightListForEach xs = foldr (+) 0 $ loop $ forEach xs

sumLeftVector :: Vector Int -> Int
sumLeftVector = V.foldl' (+) 0

sumLeftVectorForEach :: Vector Int -> Int
sumLeftVectorForEach xs = foldl' (+) 0 $ loop $ forEach xs

sumLeftVectorForEachU2 :: Vector Int -> Int
sumLeftVectorForEachU2 xs = foldl' (+) 0 $ loop $ forEachU unroll2 xs

sumLeftVectorForEachU8 :: Vector Int -> Int
sumLeftVectorForEachU8 xs = foldl' (+) 0 $ loop $ forEachU unroll8 xs

sumRightVector :: Vector Int -> Int
sumRightVector = V.foldr (+) 0

sumRightVectorForEach :: Vector Int -> Int
sumRightVectorForEach xs = foldr (+) 0 $ loop $ forEach xs

sumVectorST :: Vector Int -> Int
sumVectorST xs = runST $ do
    acc <- newSTRef 0
    exec_ $ loopT $ do
        x <- forEach xs
        lift $ modifySTRef' acc (+ x)
    readSTRef acc
