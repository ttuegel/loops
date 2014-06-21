module Test.List where

import Data.Foldable
import Prelude hiding (foldr)
import Test.Tasty.QuickCheck

import Control.Monad.Loop

sumLeft :: [Int] -> Property
sumLeft xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEach xs)

sumRight :: [Int] -> Property
sumRight xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEach xs)

sumLeftU :: [Int] -> Property
sumLeftU xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEachU unroll8 xs)

sumRightU :: [Int] -> Property
sumRightU xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEachU unroll8 xs)
