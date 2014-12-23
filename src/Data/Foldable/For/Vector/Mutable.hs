{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Foldable.For.Vector.Mutable where

import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV

import Data.Foldable.For

forSlices
  :: MVector v a
  => Int -- ^ length of each slice
  -> v s a
  -> For (v s a)
forSlices = \n v0 -> F v0 (not . MV.null) $ \v1 ->
    let (!v, !v2) = MV.splitAt n v1 in (# v2, v #)
{-# INLINE forSlices #-}
