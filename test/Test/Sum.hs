{-# LANGUAGE DataKinds #-}

module Test.Sum where

import qualified Control.Monad.Loop as LoopT
import qualified Control.Monad.Loop.Unroll as Unroll
import Data.Foldable
import Prelude hiding (foldr)
import Test.Tasty.QuickCheck

prop_sum_foldl_LoopT :: [Int] -> Property
prop_sum_foldl_LoopT xs =
    foldl' (+) 0 xs === foldl' (+) 0 (LoopT.forEach xs :: LoopT.Loop Int)

prop_sum_foldr_LoopT :: [Int] -> Property
prop_sum_foldr_LoopT xs =
    foldr (+) 0 xs === foldr (+) 0 (LoopT.forEach xs :: LoopT.Loop Int)

prop_sum_foldl_LoopT_Unroll :: [Int] -> Property
prop_sum_foldl_LoopT_Unroll xs =
    foldl' (+) 0 xs === foldl' (+) 0 (Unroll.forEach unroll xs :: Unroll.Loop Int)
  where
    unroll :: Unroll.Unroll 8
    unroll = Unroll.Unroll

prop_sum_foldr_LoopT_Unroll :: [Int] -> Property
prop_sum_foldr_LoopT_Unroll xs =
    foldr (+) 0 xs === foldr (+) 0 (Unroll.forEach unroll xs :: Unroll.Loop Int)
  where
    unroll :: Unroll.Unroll 8
    unroll = Unroll.Unroll
