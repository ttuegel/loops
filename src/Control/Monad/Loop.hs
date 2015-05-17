{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Loop where

import qualified Control.Monad.Trans.State.Lazy as StL
import qualified Control.Monad.Trans.State.Strict as StS
import Data.Foldable
import Data.Vector.Fusion.Util

newtype Loop a = Loop { runLoop :: forall r. (a -> r -> r) -> r -> r }

instance Functor Loop where
  {-# INLINE fmap #-}
  fmap = \f as ->
    Loop (\yieldB ->
            let yieldA a r = yieldB (f a) r
            in runLoop as yieldA)

instance Applicative Loop where
  {-# INLINE pure #-}
  pure = \a -> Loop (\yield r -> yield a r)

  {-# INLINE (<*>) #-}
  (<*>) = \fs as ->
    Loop (\yieldB ->
            let yieldF = \f -> runLoop as (yieldA f)
                yieldA f = \a -> yieldB (f a)
            in runLoop fs yieldF)

instance Monad Loop where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  (>>=) = \as f ->
    Loop (\yieldB ->
            let yieldA = \a -> runLoop (f a) yieldB
            in runLoop as yieldA)

instance Foldable Loop where
  {-# INLINE foldr #-}
  foldr = \f r as -> runLoop as f r

  {-
  {-# INLINE foldl' #-}
  foldl' = \f r as ->
    let yield = \a h !s -> let !t = f s a in h t
    in runLoop as yield id r
  -}

empty :: Loop a
{-# INLINE empty #-}
empty = Loop (\_ r -> r)

for :: a -> (a -> Bool) -> (a -> a) -> Loop a
{-# INLINE for #-}
for a check next =
  Loop (\yield ->
          let for_loop b | check b = \r -> yield b $ for_loop (next b) r
                         | otherwise = \r -> r
          in for_loop a)

enumFromStepN :: Num a => a -> a -> Int -> Loop a
{-# INLINE enumFromStepN #-}
enumFromStepN !x !y !n =
  fst <$> for (x, n) (\(_, m) -> m > 0) (\(w, m) -> (w + y, m - 1))
