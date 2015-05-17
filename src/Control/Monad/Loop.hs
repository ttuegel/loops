{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Loop where

import Data.Foldable

newtype Loop a = Loop { runLoop :: forall r. (a -> Loop a -> r -> r) -> r -> r }

instance Functor Loop where
  {-# INLINE fmap #-}
  fmap = fmap_go where
    fmap_go f loopA = Loop fmap_loop where
      fmap_loop yieldB = runLoop loopA (\a l r -> yieldB (f a) (fmap_go f l) r)

instance Applicative Loop where
  {-# INLINE pure #-}
  pure = pure_go where
    pure_go a = Loop pure_loop where
      pure_loop yield = yield a empty

  {-# INLINE (<*>) #-}
  (<*>) = ap_go where
    ap_go fs as = Loop ap_loop where
      ap_loop yieldB =
        runLoop fs $ \f fs' -> runLoop (ap_go fs' as) yieldB . runLoop (fmap f as) yieldB

instance Monad Loop where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  (>>=) = bind_go where
    bind_go as f = Loop bind_loop where
      bind_loop yieldB =
        runLoop as $ \a as' -> runLoop (bind_go as' f) yieldB . runLoop (f a) yieldB

instance Foldable Loop where
  {-# INLINE foldr #-}
  foldr = foldr_go where
    foldr_go f r as = runLoop as (\a as' g -> g . f a . (\r' -> foldr_go f r' as')) id r

  {-# INLINE foldl' #-}
  foldl' = foldl'_go where
    foldl'_go f !r as = runLoop as foldl'_yield r where
      foldl'_yield a as' !s = let !s' = f s a in foldl'_go f s' as'

empty :: Loop a
empty = Loop $ \_ r -> r

for :: a -> (a -> Bool) -> (a -> a) -> Loop a
for a check next = Loop for_loop
  where
    for_loop yield r | check a = yield a (for (next a) check next) r
                     | otherwise = r
