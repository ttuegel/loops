{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Loop where

import Data.Foldable


-- Technically, we could get away with just a right fold because left folds can
-- be expressed as right folds. However, GHC is bad at optimizing the function
-- composition. This way, the important optimization is constructor specialization;
-- GHC is good at that.
newtype Loop a = Loop { runLoop :: forall r.
                                   Either (r -> a -> r) (a -> r -> r)
                                   -- ^ The fold or iteratee, depending on your preferred
                                   -- terminology. 'Either' a 'Left'-fold or a 'Right'-fold;
                                   -- the loop associates accordingly.
                                -> r
                                -> r
                      }

instance Functor Loop where
  {-# INLINE [1] fmap #-}
  fmap f as = Loop fmap_loop where
    {-# INLINE [0] fmap_loop #-}
    fmap_loop y r = runLoop as yieldA r where
      yieldA =
        case y of
          Left yieldB -> Left $ \s a -> yieldB s (f a)
          Right yieldB -> Right $ \a s -> yieldB (f a) s

instance Applicative Loop where
  {-# INLINE [1] pure #-}
  pure a = Loop pure_loop where
    {-# INLINE [0] pure_loop #-}
    pure_loop y r =
      case y of
        Left yield -> yield r a
        Right yield -> yield a r

  {-# INLINE [1] (<*>) #-}
  (<*>) fs as = Loop ap_loop
    where
      {-# INLINE [0] ap_loop #-}
      ap_loop y r = runLoop fs yieldF r where
        yieldF =
          case y of
            Left yieldB ->
              let yieldA f = Left $ \s a -> yieldB s (f a)
              in Left $ \s f -> runLoop as (yieldA f) s
            Right yieldB ->
              let yieldA f = Right $ \a s -> yieldB (f a) s
              in Right $ \f s -> runLoop as (yieldA f) s

instance Monad Loop where
  {-# INLINE return #-}
  return = pure

  {-# INLINE [1] (>>=) #-}
  (>>=) as f = Loop bind_loop
    where
      {-# INLINE [0] bind_loop #-}
      bind_loop y r =
        case y of
          Left _ ->
            let yieldA = Left $ \s a -> runLoop (f a) y s
            in runLoop as yieldA r
          Right _ ->
            let yieldA = Right $ \a s -> runLoop (f a) y s
            in runLoop as yieldA r

instance Foldable Loop where
  {-# INLINE foldr #-}
  foldr f r as = runLoop as (Right f) r

  {-# INLINE foldl' #-}
  foldl' f r as = runLoop as (Left g) r
    where g !s a = f s a

empty :: Loop a
{-# INLINE empty #-}
empty = Loop (\_ r -> r)

for :: a -> (a -> Bool) -> (a -> a) -> Loop a
{-# INLINE [1] for #-}
for a check next = Loop for_loop
  where
    {-# INLINE [0] for_loop #-}
    for_loop y r =
      case y of
        Left yield -> for_loop_left yield r
        Right yield -> for_loop_right yield r
    {-# INLINE [0] for_loop_left #-}
    for_loop_left yield r =
      let for_loop_left_go s b
            | check b = for_loop_left_go (yield s b) (next b)
            | otherwise = s
      in for_loop_left_go r a
    {-# INLINE [0] for_loop_right #-}
    for_loop_right yield r =
      let for_loop_right_go b
            | check b = yield b (for_loop_right_go (next b))
            | otherwise = r
      in for_loop_right_go a

enumFromStepN :: Num a => a -> a -> Int -> Loop a
{-# INLINE enumFromStepN #-}
enumFromStepN !x !y !n =
  fst <$> for (x, n) (\(_, m) -> m > 0) (\(w, m) -> (w + y, m - 1))
