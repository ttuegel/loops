{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Loop where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.State.Strict as State
import Data.Foldable
import Data.Functor.Identity
import GHC.Types (SPEC(..))

newtype LoopT m a =
  LoopT { runLoopT :: forall r t. (MonadTrans t, Monad (t m))
                   => SPEC -> (a -> r -> t m r) -> r -> t m r }

type Loop = LoopT Identity

instance Functor (LoopT m) where
  {-# INLINE fmap #-}
  fmap = \f loop ->
    LoopT $ \ !spec yield r0 ->
      runLoopT loop spec (\a r1 -> yield (f a) r1) r0

instance Applicative (LoopT m) where
  {-# INLINE pure #-}
  pure = \a -> LoopT $ \ !_ yield r -> yield a r

  {-# INLINE (<*>) #-}
  (<*>) = \loopF loopA ->
    LoopT $ \ !spec yieldB r0 ->
      let runLoopB = \f r1 -> runLoopT (fmap f loopA) spec yieldB r1
      in runLoopT loopF spec runLoopB r0

instance Monad (LoopT m) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  (>>=) = \loopA kleisliAB ->
    LoopT $ \ !spec yieldB r0 ->
      let runLoopB = \a r1 -> runLoopT (kleisliAB a) spec yieldB r1
      in runLoopT loopA spec runLoopB r0

instance MonadTrans LoopT where
  {-# INLINE lift #-}
  lift = \inner -> LoopT $ \ !_ yield r -> lift inner >>= \a -> yield a r

instance Foldable (LoopT Identity) where
  {-# INLINE foldr #-}
  foldr = \f r loopA ->
    runIdentity $ runIdentityT $ runLoopT loopA SPEC (\a r' -> pure (f a r')) r

  {-# INLINE foldl' #-}
  foldl' = \f r loopA ->
    let foldl'_go = \a () -> do
          !r0 <- State.get
          let !r1 = f r0 a
          State.put r1
    in runIdentity $! State.execStateT (runLoopT loopA SPEC foldl'_go ()) r

empty :: Applicative f => LoopT f a
{-# INLINE empty #-}
empty = LoopT (\ !_ _ r -> pure r)

data Step s a
  = Yield a s
  | Skip s
  | Done

unfold :: Monad m => (s -> m (Step s a)) -> s -> LoopT m a
{-# INLINE unfold #-}
unfold = \step s0 -> LoopT $ \ !spec yield r0 ->
  let unfoldT_loop !spec' s1 = do
        st <- lift $ step s1
        case st of
          Yield a s2 -> yield a =<< unfoldT_loop spec' s2
          Skip s2 -> unfoldT_loop spec' s2
          Done -> pure r0
  in unfoldT_loop spec s0

for :: Monad m => a -> (a -> Bool) -> (a -> a) -> LoopT m a
{-# INLINE for #-}
for = \a check next -> unfold (\s -> pure $ if check s then Yield s (next s) else Done) a

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> LoopT m a
{-# INLINE enumFromStepN #-}
enumFromStepN = \ !x !y !n ->
  let step (w, m)
        | m > 0 = let v = w + y in pure $ Yield v (v, m - 1)
        | otherwise = pure Done
  in unfold step (x, n)
