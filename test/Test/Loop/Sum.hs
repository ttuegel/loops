module Test.Loop.Sum where

import Control.Loop
import Data.Foldable
import Data.Strict.Maybe
import Data.Strict.Tuple
import Prelude hiding (Maybe(..), foldr)
import Test.Tasty.QuickCheck

fromList :: [a] -> Loop a
fromList = unfoldl go
  where
    go [] = Nothing
    go (x:xs) = Just $! xs :!: x

foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList = foldl'

foldlLoop :: (b -> a -> b) -> b -> Loop a -> b
foldlLoop = foldl'

foldrList :: (b -> a -> a) -> a -> [b] -> a
foldrList = foldr

foldrLoop :: (b -> a -> a) -> a -> Loop b -> a
foldrLoop = foldr

prop_sum_foldl :: [Int] -> Property
prop_sum_foldl xs = foldlList (+) 0 xs === foldlLoop (+) 0 (fromList xs)

prop_sum_foldr :: [Int] -> Property
prop_sum_foldr xs = foldrList (+) 0 xs === foldrLoop (+) 0 (fromList xs)
