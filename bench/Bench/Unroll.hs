module Bench.Unroll where

import Data.Foldable
import Prelude hiding (foldr)

import Control.Monad.Loop.Unroll

sumLeft :: Unrolling n => Static n -> Int -> Int
{-# INLINE sumLeft #-}
sumLeft unr = \n -> foldl' (+) 0 $ loop $ for 0 (<= n) (+ 1)

sumLeft1, sumLeft2, sumLeft4, sumLeft8 :: Int -> Int
sumLeft1 = sumLeft unroll1
sumLeft2 = sumLeft unroll2
sumLeft4 = sumLeft unroll4
sumLeft8 = sumLeft unroll8

sumRight :: Unrolling n => Static n -> Int -> Int
{-# INLINE sumRight #-}
sumRight unr = \n -> foldr (+) 0 $ loop $ for 0 (<= n) (+ 1)

sumRight1, sumRight2, sumRight4, sumRight8 :: Int -> Int
sumRight1 = sumRight unroll1
sumRight2 = sumRight unroll2
sumRight4 = sumRight unroll4
sumRight8 = sumRight unroll8
