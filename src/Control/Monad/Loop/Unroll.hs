module Control.Monad.Loop.Unroll where

import Control.Applicative
import GHC.TypeLits

import Control.Monad.Loop.Internal
import Control.Monad.Loop.Static

numFromNU
    :: (Integral a, KnownNat n, Num a, Ord a, Unrolling n)
    => Static n -> a -> a -> LoopR r m a
{-# INLINE numFromNU #-}
numFromNU unr = \a0 n ->
    let step = fromIntegral $ natVal unr
        end = a0 + n
        bigEnd = end - (n `mod` step)
        big = do
            i <- for a0 (< bigEnd) (+ step)
            iterateS unr i (+ 1)
        little = for bigEnd (< end) (+ 1)
    in big <|> little
