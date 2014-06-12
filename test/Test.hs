module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Test.Sum

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "sum"
      [ QC.testProperty "foldl" prop_sum_foldl_LoopT
      , QC.testProperty "foldr" prop_sum_foldr_LoopT
      , QC.testProperty "foldl (Unroll)" prop_sum_foldl_LoopT_Unroll
      , QC.testProperty "foldr (Unroll)" prop_sum_foldr_LoopT_Unroll
      ]
  ]
