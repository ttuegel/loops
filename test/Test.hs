module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Test.Loop.Sum

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Loop"
      [ testGroup "Sum"
          [ QC.testProperty "foldl" prop_sum_foldl
          , QC.testProperty "foldr" prop_sum_foldr
          ]
      ]
  ]
