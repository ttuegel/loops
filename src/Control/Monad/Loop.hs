{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Loop where

import Data.Foldable
import GHC.Types (SPEC(..))

-- Technically, we could get away with just a right fold because left folds can
-- be expressed as right folds. However, GHC is bad at optimizing the function
-- composition. This way, the important optimization is constructor specialization;
-- GHC is good at that.
newtype Loop a = Loop { runLoop :: forall r.
                                   SPEC
                                -> Either (r -> a -> r) (a -> r -> r)
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
    in Loop $ \ !spec y -> runLoop as spec $ both fmap_yield_left fmap_yield_right y

instance Applicative Loop where
  {-# INLINE pure #-}
  pure = \a -> Loop $ \ !_ y r ->
    let pure_loop_left = \yield -> yield r a
        pure_loop_right = \yield -> yield a r
    in either pure_loop_left pure_loop_right y

  {-# INLINE (<*>) #-}
  (<*>) = \fs as -> Loop $ \ !spec y ->
    let ap_loop_left = \yieldB r f ->
          runLoop as spec (Left $ \s a -> yieldB s (f a)) r
        ap_loop_right = \yieldB f r ->
          runLoop as spec (Right $ \a s -> yieldB (f a) s) r
    in runLoop fs spec $ both ap_loop_left ap_loop_right y

instance Monad Loop where
  {-# INLINE return #-}
  return = pure

  {-# INLINE [1] (>>=) #-}
  (>>=) = \as f -> Loop $ \ !spec y ->
    let bind_loop_left = \_ r a -> runLoop (f a) spec y r
        bind_loop_right = \_ a r -> runLoop (f a) spec y r
    in runLoop as spec $ both bind_loop_left bind_loop_right y

instance Foldable Loop where
  {-# INLINE foldr #-}
  foldr f r as = runLoop as SPEC (Right f) r

  {-# INLINE foldl' #-}
  foldl' f r as = runLoop as SPEC (Left g) r
    where g !s a = f s a

empty :: Loop a
{-# INLINE empty #-}
empty = Loop (\ !_ _ r -> r)

data Step s a
  = Yield a s
  | Skip s
  | Done

unfold :: (s -> Step s a) -> s -> Loop a
{-# INLINE unfold #-}
unfold = \step s0 -> Loop $ \ !_ y r0 ->

  let unfold_loop_left = \yield ->
        let unfold_loop_left_go !spec s1 r =
              case step s1 of
                Yield a s2 -> unfold_loop_left_go spec s2 (yield r a)
                Skip s2 -> unfold_loop_left_go spec s2 r
                Done -> r
        in unfold_loop_left_go SPEC s0 r0

      unfold_loop_right = \yield ->
        let unfold_loop_right_go !spec s1 =
              case step s1 of
                Yield a s2 -> yield a (unfold_loop_right_go spec s2)
                Skip s2 -> unfold_loop_right_go spec s2
                Done -> r0
        in unfold_loop_right_go SPEC s0

  in either unfold_loop_left unfold_loop_right y

for :: a -> (a -> Bool) -> (a -> a) -> Loop a
{-# INLINE for #-}
for = \a check next -> unfold (\s -> if check s then Yield s (next s) else Done) a

enumFromStepN :: Num a => a -> a -> Int -> Loop a
{-# INLINE enumFromStepN #-}
enumFromStepN = \ !x !y !n ->
  let step (w, m)
        | m > 0 = let v = w + y in Yield v (v, m - 1)
        | otherwise = Done
  in unfold step (x, n)

both :: (a -> c) -> (b -> d) -> Either a b -> Either c d
{-# INLINE both #-}
both fl fr = either (Left . fl) (Right . fr)
