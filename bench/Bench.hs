module Main where

import Criterion.Main
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import Prelude hiding (foldr)

import Control.Monad.Loop.Unroll

main :: IO ()
main = defaultMain
    [ bgroup "left"
        {-
        - The foldl benchmarks are for pure performance. "Loop 1"
        - should perform on par with "Vector". The unrolled loops
        - should be faster, but subsequent performance gains are
        - expected to diminish. Unrolling a foldl' loop only prevents
        - a jump at the end of each iteration. The biggest gain will
        - come from improved branch prediction, but that doesn't scale
        - beyond a few unrollings.
        -}
        [ bench "sumLeftList" $ nf sumLeftList iters
        , bench "sumManual" $ nf sumManual iters
        , bench "sumLeftVector" $ nf sumLeftVector iters
        , bench "sumLeft1" $ nf sumLeft1 iters
        , bench "sumLeft2" $ nf sumLeft2 iters
        , bench "sumLeft4" $ nf sumLeft4 iters
        , bench "sumLeft8" $ nf sumLeft8 iters
        ]
    , bgroup "foldr"
        {-
        - The foldr benchmarks will show pretty poor performance
        - because foldr creates a new thunk for every iteration of the
        - loop. (This is common to foldr over all structures.) On the
        - other hand, the gains from unrolling will be much greater
        - than with foldl' because each subsequent unrolling prevents
        - another thunk from being allocated. (Thunk allocation is much
        - more expensive than just a jump.
        -}
        [ bench "sumRightList" $ nf sumRightList iters
        , bench "sumRightVector" $ nf sumRightVector iters
        , bench "sumRight1" $ nf sumRight1 iters
        , bench "sumRight2" $ nf sumRight2 iters
        , bench "sumRight4" $ nf sumRight4 iters
        , bench "sumRight8" $ nf sumRight8 iters
        ]
    ]
  where
    iters :: Int
    iters = 10000000

sumLeft :: Unrolling n => Unroll n -> Int -> Int
{-# INLINE sumLeft #-}
sumLeft unr = \n -> foldl' (+) 0 $ loop $ for unr 0 (<= n) (+ 1)

sumLeft1, sumLeft2, sumLeft4, sumLeft8 :: Int -> Int
sumLeft1 = sumLeft unroll1
sumLeft2 = sumLeft unroll2
sumLeft4 = sumLeft unroll4
sumLeft8 = sumLeft unroll8

sumLeftList :: Int -> Int
sumLeftList n = foldl' (+) 0 [0..n]

sumLeftVector :: Int -> Int
sumLeftVector n = V.foldl' (+) 0 $ V.enumFromTo 0 n

sumRightList :: Int -> Int
sumRightList n = foldr (+) 0 [0..n]

sumRightVector :: Int -> Int
sumRightVector n =
    V.foldr (+) 0 $ V.enumFromTo 0 n

sumRight :: Unrolling n => Unroll n -> Int -> Int
{-# INLINE sumRight #-}
sumRight unr = \n -> foldr (+) 0 $ loop $ for unr 0 (<= n) (+ 1)

sumRight1, sumRight2, sumRight4, sumRight8 :: Int -> Int
sumRight1 = sumRight unroll1
sumRight2 = sumRight unroll2
sumRight4 = sumRight unroll4
sumRight8 = sumRight unroll8

sumManual :: Int -> Int
sumManual n = go 0 0
  where
    go acc i | i <= n = go (acc + i) (i + 1)
             | otherwise = acc
