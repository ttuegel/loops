module Test.List where

import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import Data.Foldable
import Data.STRef
import Prelude hiding (foldr)
import Test.Tasty.QuickCheck

import Control.Monad.Loop

sumLeft :: [Int] -> Property
sumLeft xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEach xs)

sumRight :: [Int] -> Property
sumRight xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEach xs)

sumST :: [Int] -> Property
sumST xs =
    foldl' (+) 0 xs === fromST
  where
    fromST = runST $ do
        acc <- newSTRef 0
        exec_ $ loopT $ do
            x <- forEach xs
            lift $ modifySTRef' acc (+ x)
        readSTRef acc

sumLeftU :: [Int] -> Property
sumLeftU xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEachU unroll8 xs)

sumRightU :: [Int] -> Property
sumRightU xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEachU unroll8 xs)

sumSTU :: [Int] -> Property
sumSTU xs =
    foldl' (+) 0 xs === fromST
  where
    fromST = runST $ do
        acc <- newSTRef 0
        exec_ $ loopT $ do
            x <- forEachU unroll8 xs
            lift $ modifySTRef' acc (+ x)
        readSTRef acc
