module Main where

import Criterion.Main
import Data.Foldable
import Data.Functor.Identity

import Control.Monad.Loop (Loop, LoopT)
import qualified Control.Monad.Loop as L
import Data.Vector.Fusion.Stream (Stream)
import qualified Data.Vector.Fusion.Stream as S

it_length :: (Int -> Int -> Int)
{-# INLINE it_length #-}
it_length len _ = len + 1

flat :: Int -> Loop Int
{-# INLINE flat #-}
flat = \n -> L.enumFromStepN 0 1 n

nested2 :: Int -> Loop Int
{-# INLINE nested2 #-}
nested2 = \n -> do
  _ <- L.enumFromStepN (0 :: Int) 1 n
  L.enumFromStepN 0 1 n

nested3 :: Int -> Loop Int
{-# INLINE nested3 #-}
nested3 = \n -> do
  _ <- L.enumFromStepN (0 :: Int) 1 n
  _ <- L.enumFromStepN (0 :: Int) 1 n
  L.enumFromStepN 0 1 n

main :: IO ()
main =
  defaultMain
  [ bench "Stream" $ nf (S.foldl' it_length 0 . S.enumFromStepN 0 1) (1000000 :: Int)
  , bench "Loop/flat" $ nf (foldl' it_length 0 . flat) (1000000 :: Int)
  , bench "Loop/nested2" $ nf (foldl' it_length 0 . nested2) (1000 :: Int)
  , bench "Loop/nested3" $ nf (foldl' it_length 0 . nested3) (100 :: Int)
  ]
