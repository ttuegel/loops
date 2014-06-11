{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Loop where

import Control.Applicative
import Control.Category ((<<<), (>>>))
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Functor.Identity
import Data.Profunctor (lmap)
import Prelude hiding (foldr)

newtype LoopT m a =
    LoopT { runLoopT :: forall r. (a -> (r -> m r) -> (r -> m r))
                     -> (r -> m r)
                     -> (r -> m r) }

instance Functor (LoopT m) where
    {-# INLINE fmap #-}
    fmap f loop = LoopT $ \yield next -> runLoopT loop (lmap f yield) next

instance Applicative (LoopT m) where
    {-# INLINE pure #-}
    pure a = LoopT $ \yield -> yield a
    {-# INLINE (<*>) #-}
    fs <*> as = LoopT $ \yield -> runLoopT fs $ \f -> runLoopT (fmap f as) yield

instance Monad (LoopT m) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    as >>= f = LoopT $ \yield -> runLoopT as $ \a -> runLoopT (f a) yield

instance MonadTrans LoopT where
    {-# INLINE lift #-}
    lift m = LoopT $ \yield next r -> m >>= \a -> yield a next r

instance MonadIO m => MonadIO (LoopT m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

exec_ :: Monad m => LoopT m a -> m ()
{-# INLINE exec_ #-}
exec_ loop = runLoopT loop (\_ next -> next) return ()

for :: a -> (a -> Bool) -> (a -> a) -> LoopT m a
{-# INLINE for #-}
{-
- The body of this loop was originally:
-
->  let go a | cond a = yield a $ go $ adv a
->           | otherwise = next
->  in go start
-
- but GHC needed -fspec-constr (-O2) to optimize correctly. In particular,
- the strictness of the accumulator in foldl' was not being detected; the
- loop would box and unbox the accumulator on every iteration. Rather than
- count on users to enable particular flags, I thought it made more sense
- (for a simple function) to perform the call-pattern specialization by
- hand. This induces a small overhead in empty loops.
-}
for start cond adv
    | cond start = LoopT $ \yield next ->
        let go a = yield a $ let a' = adv a in if cond a' then go a' else next
        in go start
    | otherwise = continue_

continue :: a -> LoopT m a
{-# INLINE continue #-}
continue a = LoopT $ \yield next -> yield a next

continue_ :: LoopT m a
{-# INLINE continue_ #-}
continue_ = LoopT $ \_ next -> next
