module Test.Sum where

import qualified Control.Monad.Loop as LoopT
import Data.Foldable
import Prelude hiding (foldr)
import Test.Tasty.QuickCheck

prop_sum_foldl_LoopT :: [Int] -> Property
prop_sum_foldl_LoopT xs =
    foldl' (+) 0 xs === foldl' (+) 0 (LoopT.forEach xs :: LoopT.Loop Int)

prop_sum_foldr_LoopT :: [Int] -> Property
prop_sum_foldr_LoopT xs =
    foldr (+) 0 xs === foldr (+) 0 (LoopT.forEach xs :: LoopT.Loop Int)
