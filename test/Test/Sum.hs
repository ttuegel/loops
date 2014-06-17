{-# LANGUAGE DataKinds #-}

module Test.Sum where

import Control.Monad.Loop.Unroll
import Data.Foldable
import Prelude hiding (foldr)
import Test.Tasty.QuickCheck

prop_sum_foldl_LoopT :: [Int] -> Property
prop_sum_foldl_LoopT xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEach unroll1 xs)

prop_sum_foldr_LoopT :: [Int] -> Property
prop_sum_foldr_LoopT xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEach unroll1 xs)

prop_sum_foldl_LoopT_Unroll :: [Int] -> Property
prop_sum_foldl_LoopT_Unroll xs =
    foldl' (+) 0 xs === (foldl' (+) 0 $ loop $ forEach unroll8 xs)

prop_sum_foldr_LoopT_Unroll :: [Int] -> Property
prop_sum_foldr_LoopT_Unroll xs =
    foldr (+) 0 xs === (foldr (+) 0 $ loop $ forEach unroll8 xs)

prop_break_order :: [Int] -> Property
prop_break_order xs =
    foldl' (+) 0 before === foldl' (+) 0 after
  where
    before :: Loop Int
    before = loop $ breaking_ $ \break_ -> do
      x <- forEach unroll1 xs
      if x < 10 then continue 10 else break_
    after :: Loop Int
    after = loop $ breaking_ $ \break_ -> do
      x <- forEach unroll1 xs
      return ()
      if x < 10 then continue 10 else break_
