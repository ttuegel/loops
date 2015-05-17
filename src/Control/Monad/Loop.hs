{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Loop where

import qualified Control.Monad.Trans.State.Lazy as StL
import qualified Control.Monad.Trans.State.Strict as StS
import Data.Foldable

newtype Loop a = Loop { runLoop :: forall f. Applicative f => (a -> f ()) -> f () }

instance Functor Loop where
  {-# INLINE fmap #-}
  fmap = \f as -> Loop (\yield -> runLoop as (yield . f))

instance Applicative Loop where
  {-# INLINE pure #-}
  pure = \a -> Loop (\yield -> yield a)

  {-# INLINE (<*>) #-}
  (<*>) = \fs as -> Loop (\yield -> runLoop fs (\f -> runLoop as (yield . f)))

instance Monad Loop where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  (>>=) = \as f -> Loop (\yield -> runLoop as (\a -> runLoop (f a) yield))

instance Foldable Loop where
  {-# INLINE foldr #-}
  foldr = \f r as ->
    let foldr_go a = StL.modify (. f a)
    in StL.execState (runLoop as foldr_go) id r

  {-# INLINE foldl' #-}
  foldl' = \f r as ->
    let foldl'_go a = do
          !s <- StS.get
          let !t = f s a
          StS.put t
    in StS.execState (runLoop as foldl'_go) r

empty :: Loop a
{-# INLINE empty #-}
empty = Loop (\_ -> pure ())

for :: a -> (a -> Bool) -> (a -> a) -> Loop a
{-# INLINE for #-}
for a check next =
  Loop (\yield ->
          let for_loop b | check b = yield b *> for_loop (next b)
                         | otherwise = pure ()
          in for_loop a)
