module Bench.Loop where

import Data.Foldable
import Prelude hiding (foldr)

import Control.Monad.Loop

sumLeft :: Int -> Int
sumLeft = \n -> foldl' (+) 0 $ loop $ for 0 (<= n) (+ 1)

sumLeftReturn :: Int -> Int
sumLeftReturn = \n -> foldl' (+) 0 $ loop $ for 0 (<= n) (+ 1) >>= return

sumRight :: Int -> Int
sumRight = \n -> foldr (+) 0 $ loop $ for 0 (<= n) (+ 1)
