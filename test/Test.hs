module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Test.Sum

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "sum"
      [ testGroup "Loop"
          [ QC.testProperty "foldl" prop_sum_foldl_Loop
          , QC.testProperty "foldr" prop_sum_foldr_Loop
          ]
      , testGroup "LoopT"
          [ QC.testProperty "foldl" prop_sum_foldl_LoopT
          , QC.testProperty "foldr" prop_sum_foldr_LoopT
          ]
      ]
  ]
