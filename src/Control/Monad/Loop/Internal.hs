{-# LANGUAGE RankNTypes #-}

module Control.Monad.Loop.Internal where

import Control.Applicative (Applicative(..), (<$>))
import Control.Category ((<<<), (>>>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe (fromJust, isJust)
import Data.Profunctor (lmap)
import Prelude hiding (foldr)

newtype LoopT m a =
    LoopT { runLoopT :: forall r. (a -> m r -> m r -> m r)
                     -> m r -> m r -> m r }

type Loop = LoopT Identity

instance Functor (LoopT m) where
    {-# INLINE fmap #-}
    fmap f loop = LoopT $ \yield -> runLoopT loop (lmap f yield)

instance Applicative (LoopT m) where
    {-# INLINE pure #-}
    pure a = LoopT $ \yield -> yield a
    {-# INLINE (<*>) #-}
    fs <*> as = LoopT $ \yield next ->
        runLoopT fs (\f next' _ -> runLoopT (fmap f as) yield next' next) next

instance Monad (LoopT m) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    as >>= f = LoopT $ \yield next ->
        runLoopT as (\a next' _ -> runLoopT (f a) yield next' next) next

instance MonadTrans LoopT where
    {-# INLINE lift #-}
    lift m = LoopT $ \yield next brk -> m >>= \a -> yield a next brk

instance MonadIO m => MonadIO (LoopT m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

instance (Applicative m, Foldable m) => Foldable (LoopT m) where
    {-# INLINE foldr #-}
    foldr f r xs = foldr (<<<) id inner r
      where
        inner = runLoopT xs (\a next _ -> (f a <<<) <$> next) (pure id) (pure id)

    {-# INLINE foldl' #-}
    foldl' f r xs = foldl' (!>>>) id inner r
      where
        (!>>>) h g = h >>> (g $!)
        inner = runLoopT xs (\a next _ -> (flip f a >>>) <$> next) (pure id) (pure id)

exec_ :: Monad m => LoopT m a -> m ()
{-# INLINE exec_ #-}
exec_ loop = runLoopT loop (\_ next _ -> next) (return ()) (return ())

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
    | cond start = LoopT $ \yield next _ ->
        let yield' a r = yield a r next
            go a = yield' a $ let a' = adv a in if cond a' then go a' else next
        in go start
    | otherwise = continue_

unfoldl :: (i -> Maybe (i, a)) -> i -> LoopT m a
{-# INLINE unfoldl #-}
unfoldl unf i0 = fromJust . fmap snd <$> for (unf i0) isJust (>>= unf . fst)

continue :: a -> LoopT m a
{-# INLINE continue #-}
continue a = LoopT $ \yield next -> yield a next

continue_ :: LoopT m a
{-# INLINE continue_ #-}
continue_ = LoopT $ \_ next _ -> next

break_ :: Loop a
{-# INLINE break_ #-}
break_ = LoopT $ \_ _ brk -> brk
