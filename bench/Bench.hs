module Main where

import Criterion.Main
import Data.Foldable
import Data.Functor.Identity

import Control.Monad.Loop (Loop)
import qualified Control.Monad.Loop as L
import Data.Vector.Fusion.Stream (Stream)
import qualified Data.Vector.Fusion.Stream as S

it_length :: (Int -> Int -> Int)
{-# INLINE it_length #-}
it_length len _ = len + 1

flat_Loop :: Int -> Int
flat_Loop n = foldl' it_length 0 loop
  where
    loop :: Loop Identity Int
    loop = L.enumFromStepN 0 1 n

flat_Stream :: Int -> Int
flat_Stream n = S.foldl' it_length 0 stream
  where
    stream :: Stream Int
    stream = S.enumFromStepN 0 1 n

{-
bind2 :: Int -> Loop Identity Int
{-# INLINE bind2 #-}
bind2 = \n -> do
  _ <- L.enumFromStepN (0 :: Int) 1 n
  L.enumFromStepN 0 1 n
-}

ap2_Loop :: Int -> Int
ap2_Loop n = foldl' it_length 0 loop
  where
    loop :: Loop Identity Int
    loop = (\_ x -> x)
           <$> L.enumFromStepN (0 :: Int) 1 n
           <*> L.enumFromStepN 0 1 n

ap2_Stream :: Int -> Int
ap2_Stream n = S.foldl' inner 0 stream
  where
    stream :: Stream Int
    stream = S.enumFromStepN 0 1 n
    inner :: Int -> Int -> Int
    inner = \len _ -> S.foldl' it_length len stream

{-
bind3 :: Int -> Loop Identity Int
{-# INLINE bind3 #-}
bind3 = \n -> do
  _ <- L.enumFromStepN (0 :: Int) 1 n
  _ <- L.enumFromStepN (0 :: Int) 1 n
  L.enumFromStepN 0 1 n
-}

ap3_Loop :: Int -> Int
ap3_Loop n = foldl' it_length 0 loop
  where
    loop :: Loop Identity Int
    loop = (\_ _ x -> x)
           <$> L.enumFromStepN (0 :: Int) 1 n
           <*> L.enumFromStepN (0 :: Int) 1 n
           <*> L.enumFromStepN 0 1 n

ap3_Stream :: Int -> Int
ap3_Stream n = S.foldl' mid 0 stream
  where
    stream :: Stream Int
    stream = S.enumFromStepN 0 1 n
    mid :: Int -> Int -> Int
    mid = \len _ -> S.foldl' inner len stream
    inner :: Int -> Int -> Int
    inner = \len _ -> S.foldl' it_length len stream

main :: IO ()
main =
  defaultMain
  [ bench "Stream/flat" $ nf flat_Stream 1000000
  , bench "Loop/flat" $ nf flat_Loop 1000000
  --, bench "Loop/bind2" $ nf (foldl' it_length 0 . bind2) (1000 :: Int)
  --, bench "Loop/bind3" $ nf (foldl' it_length 0 . bind3) (100 :: Int)
  , bench "Stream/ap2" $ nf ap2_Stream 1000
  , bench "Loop/ap2" $ nf ap2_Loop 1000
  , bench "Stream/ap3" $ nf ap3_Stream 100
  , bench "Loop/ap3" $ nf ap3_Loop 100
  ]
