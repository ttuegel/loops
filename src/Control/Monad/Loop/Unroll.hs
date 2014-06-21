module Control.Monad.Loop.Unroll where

import GHC.TypeLits

import Control.Monad.Loop.Internal
import Control.Monad.Loop.Static

numFromNU
    :: (Integral a, KnownNat n, Num a, Ord a, Unrolling n)
    => Static n -> a -> a -> LoopR r m a
{-# INLINE numFromNU #-}
numFromNU unr = \a0 n -> do
    let bigStep = fromIntegral $ natVal unr
        an = a0 + n
        bigStop = a0 + (n `div` bigStep) * bigStep
    i <- for a0 (< an) (+ bigStep)
    if i < bigStop
      then iterateS unr i (+ 1)
      else for i (< an) (+ 1)
