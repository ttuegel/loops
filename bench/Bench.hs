module Main where

import Criterion.Main
import Data.Foldable

import Control.Monad.Loop (Loop)
import qualified Control.Monad.Loop as L
import Data.Vector.Fusion.Stream (Stream)
import qualified Data.Vector.Fusion.Stream as S

main :: IO ()
main =
  defaultMain
  [ bench "Loop" $ nf (\n -> foldl' (+) 0 $ L.for 0 (< n) (+ 1)) (100000 :: Int)
  , bench "Stream" $ nf (\n -> S.foldl' (+) (0 :: Int) $ S.enumFromStepN 0 1 n) (100000 :: Int)
  ]
