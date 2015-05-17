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
  {-# INLINE fmap #-}
  fmap = \f as ->
    let fmap_yield_left = \yield r a -> yield r (f a)
        fmap_yield_right = \yield a r -> yield (f a) r
    in Loop $ \y -> runLoop as $ both fmap_yield_left fmap_yield_right y

instance Applicative Loop where
  {-# INLINE pure #-}
  pure = \a -> Loop $ \y r ->
    let pure_loop_left = \yield -> yield r a
        pure_loop_right = \yield -> yield a r
    in either pure_loop_left pure_loop_right y

  {-# INLINE (<*>) #-}
  (<*>) = \fs as -> Loop $ \y ->
    let ap_loop_left = \yieldB r f ->
          runLoop as (Left $ \s a -> yieldB s (f a)) r
        ap_loop_right = \yieldB f r ->
          runLoop as (Right $ \a s -> yieldB (f a) s) r
    in runLoop fs $ both ap_loop_left ap_loop_right y

instance Monad Loop where
  {-# INLINE return #-}
  return = pure

  {-# INLINE [1] (>>=) #-}
  (>>=) = \as f -> Loop $ \y ->
    let bind_loop_left = \_ r a -> runLoop (f a) y r
        bind_loop_right = \_ a r -> runLoop (f a) y r
    in runLoop as $ both bind_loop_left bind_loop_right y

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

both :: (a -> c) -> (b -> d) -> Either a b -> Either c d
{-# INLINE both #-}
both fl fr = either (Left . fl) (Right . fr)
