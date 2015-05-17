{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Loop where

import qualified Control.Monad.Trans.State.Lazy as StL
import qualified Control.Monad.Trans.State.Strict as StS
import Data.Foldable

newtype Loop a = Loop { runLoop :: forall f. Applicative f => (a -> f ()) -> f () }

instance Functor Loop where
  {-# INLINE [1] fmap #-}
  fmap = \f as -> Loop (\yield -> runLoop as (yield . f))

instance Applicative Loop where
  {-# INLINE [1] pure #-}
  pure = \a -> Loop (\yield -> yield a)

  {-# INLINE [1] (<*>) #-}
  (<*>) = \fs as -> Loop (\yield -> runLoop fs (\f -> runLoop as (yield . f)))

instance Monad Loop where
  {-# INLINE [1] return #-}
  return = pure

  {-# INLINE [1] (>>=) #-}
  (>>=) = \as f -> Loop (\yield -> runLoop as (\a -> runLoop (f a) yield))

instance Foldable Loop where
  {-# INLINE [1] foldr #-}
  foldr = \f r as ->
    let {-# INLINE [0] foldr_go #-}
        foldr_go a = StL.modify (. f a)
    in StL.execState (runLoop as foldr_go) id r

  {-# INLINE [1] foldl' #-}
  foldl' = \f r as ->
    let {-# INLINE [0] foldl'_go #-}
        foldl'_go a = do
          !s <- StS.get
          let !t = f s a
          StS.put t
    in StS.execState (runLoop as foldl'_go) r

empty :: Loop a
{-# INLINE empty #-}
empty = Loop (\_ -> pure ())

for :: a -> (a -> Bool) -> (a -> a) -> Loop a
{-# INLINE [1] for #-}
for a check next =
  Loop (\yield ->
          let {-# INLINE [0] for_loop #-}
              for_loop b | check b = yield b *> for_loop (next b)
                         | otherwise = pure ()
          in for_loop a)

enumFromStepN :: Num a => a -> a -> Int -> Loop a
{-# INLINE enumFromStepN #-}
enumFromStepN !x !y !n =
  fst <$> for (x, n) (\(_, m) -> m > 0) (\(w, m) -> (w + y, m - 1))
