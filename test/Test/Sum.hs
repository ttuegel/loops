{-# LANGUAGE DataKinds #-}

module Test.Sum where

import Data.Foldable
import Prelude hiding (foldr)
import Test.Tasty.QuickCheck

import Control.Monad.Loop

prop_sum_foldl_LoopT :: [Int] -> Property
prop_sum_foldl_LoopT xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEach xs)

prop_sum_foldr_LoopT :: [Int] -> Property
prop_sum_foldr_LoopT xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEach xs)

prop_sum_foldl_LoopT_Unroll :: [Int] -> Property
prop_sum_foldl_LoopT_Unroll xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEachU unroll8 xs)

prop_sum_foldr_LoopT_Unroll :: [Int] -> Property
prop_sum_foldr_LoopT_Unroll xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEachU unroll8 xs)

prop_break_order :: [Int] -> Property
prop_break_order xs =
    foldl' (+) 0 before === foldl' (+) 0 after
  where
    before :: Loop Int
    before = loop $ breaking_ $ \break_ -> do
      x <- forEach xs
      if x < 10 then continue 10 else break_
    after :: Loop Int
    after = loop $ breaking_ $ \break_ -> do
      x <- forEach xs
      return ()
      if x < 10 then continue 10 else break_
