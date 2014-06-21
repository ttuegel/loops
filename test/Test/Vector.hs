module Test.Vector where

import Data.Foldable
import qualified Data.Vector as V
import Prelude hiding (foldr)
import Test.Tasty.QuickCheck

import Control.Monad.Loop

sumLeft :: [Int] -> Property
sumLeft xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEach $ V.fromList xs)

sumRight :: [Int] -> Property
sumRight xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEach $ V.fromList xs)

sumLeftU :: [Int] -> Property
sumLeftU xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEachU unroll8 $ V.fromList xs)

sumRightU :: [Int] -> Property
sumRightU xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEachU unroll8 $ V.fromList xs)
