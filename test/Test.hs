module Main where

import Data.Foldable
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Control.Monad.Loop

import qualified Test.List as List
import qualified Test.Vector as Vector

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Break"
      [ QC.testProperty "Order" breakOrder ]
  , testGroup "ForEach/List"
      [ QC.testProperty "sumLeft" List.sumLeft
      , QC.testProperty "sumRight" List.sumRight
      , QC.testProperty "sumLeftU" List.sumLeftU
      , QC.testProperty "sumRightU" List.sumRightU
      ]
  , testGroup "ForEach/Vector"
      [ QC.testProperty "sumLeft" Vector.sumLeft
      , QC.testProperty "sumRight" Vector.sumRight
      , QC.testProperty "sumLeftU" Vector.sumLeftU
      , QC.testProperty "sumRightU" Vector.sumRightU
      ]
  ]

breakOrder :: [Int] -> Property
breakOrder xs =
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
