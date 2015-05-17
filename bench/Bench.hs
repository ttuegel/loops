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

bind2 :: Int -> Loop Int
{-# INLINE bind2 #-}
bind2 = \n -> do
  _ <- L.enumFromStepN (0 :: Int) 1 n
  L.enumFromStepN 0 1 n

ap2 :: Int -> Loop Int
{-# INLINE ap2 #-}
ap2 = \n -> (\_ x -> x) <$> L.enumFromStepN (0 :: Int) 1 n <*> L.enumFromStepN 0 1 n

bind3 :: Int -> Loop Int
{-# INLINE bind3 #-}
bind3 = \n -> do
  _ <- L.enumFromStepN (0 :: Int) 1 n
  _ <- L.enumFromStepN (0 :: Int) 1 n
  L.enumFromStepN 0 1 n

ap3 :: Int -> Loop Int
{-# INLINE ap3 #-}
ap3 = \n -> (\_ _ x -> x)
            <$> L.enumFromStepN (0 :: Int) 1 n
            <*> L.enumFromStepN (0 :: Int) 1 n
            <*> L.enumFromStepN 0 1 n

main :: IO ()
main =
  defaultMain
  [ bench "Stream" $ nf (S.foldl' it_length 0 . S.enumFromStepN 0 1) (1000000 :: Int)
  , bench "Loop/flat" $ nf (foldl' it_length 0 . flat) (1000000 :: Int)
  , bench "Loop/bind2" $ nf (foldl' it_length 0 . bind2) (1000 :: Int)
  , bench "Loop/bind3" $ nf (foldl' it_length 0 . bind3) (100 :: Int)
  , bench "Loop/ap2" $ nf (foldl' it_length 0 . ap2) (1000 :: Int)
  , bench "Loop/ap3" $ nf (foldl' it_length 0 . ap3) (100 :: Int)
  ]
