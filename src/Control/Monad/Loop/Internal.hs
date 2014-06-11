{-# LANGUAGE RankNTypes #-}

module Control.Monad.Loop.Internal where

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Category ((<<<), (>>>))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe (fromJust, isJust)
import Data.Traversable (Traversable(..))
import Prelude hiding (foldr, iterate)

-- | @LoopT m a@ represents a loop over a base type @m@ that yields a value
-- @a@ at each iteration. It can be used as a monad transformer, but there
-- are actually no restrictions on the type @m@. However, this library only
-- provides functions to execute the loop if @m@ is at least 'Applicative'
-- (for 'exec_'). If @m@ is also 'Foldable', so is @LoopT m@. For any other
-- type, you may use 'runLoopT'.
newtype LoopT m a = LoopT
    { runLoopT
        :: forall r. (a -> m r -> m r -> m r)
          -- ^ Yield a value to the inner loop. The inner loop will call
          -- the second argument to continue and the third argument to
          -- break.
        -> m r  -- ^ Continue
        -> m r  -- ^ Break
        -> m r
    }

-- | @Loop@ is a pure loop, without side-effects.
type Loop = LoopT Identity

-- | @loop@ is just an aid to type inference. For loops over a base monad,
-- there are usually other constraints that fix the type, but for pure
-- loops, the compiler often has trouble inferring @Identity@.
loop :: Loop a -> Loop a
{-# INLINE loop #-}
loop = id

instance Functor (LoopT m) where
    {-# INLINE fmap #-}
    fmap f xs = LoopT $ \yield -> runLoopT xs (yield . f)

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
        yield a next _ = (f a <<<) <$> next
        inner = runLoopT xs yield (pure id) (pure id)

    {-# INLINE foldl' #-}
    foldl' f r xs = foldl' (!>>>) id inner r
      where
        (!>>>) h g = h >>> (g $!)
        yield a next _ = (flip f a >>>) <$> next
        inner = runLoopT xs yield (pure id) (pure id)

instance (Applicative m, Foldable m) => Traversable (LoopT m) where
    {-# INLINE sequenceA #-}
    sequenceA = foldr (liftA2 cons) (pure continue_)

cons :: a -> LoopT m a -> LoopT m a
{-# INLINE cons #-}
cons a as = LoopT $ \yield next brk -> yield a (runLoopT as yield next brk) next

-- | Yield a value for this iteration of the loop and skip immediately to
-- the next iteration.
continue :: a -> LoopT m a
{-# INLINE continue #-}
continue a = LoopT $ \yield next -> yield a next

-- | Skip immediately to the next iteration of the loop without yielding
-- a value.
continue_ :: LoopT m a
{-# INLINE continue_ #-}
continue_ = LoopT $ \_ next _ -> next

-- | Skip all the remaining iterations of the immediately-enclosing loop.
break_ :: LoopT m a
{-# INLINE break_ #-}
break_ = LoopT $ \_ _ brk -> brk

-- | Execute a loop, sequencing the effects and discarding the values.
exec_ :: Applicative m => LoopT m a -> m ()
{-# INLINE exec_ #-}
exec_ xs = runLoopT xs (\_ next _ -> next) (pure ()) (pure ())

-- | Iterate forever (or until 'break' is used).
iterate
    :: a          -- ^ Starting value of iterator
    -> (a -> a)   -- ^ Advance the iterator
    -> LoopT m a
{-# INLINE iterate #-}
iterate a0 adv = LoopT $ \yield next _ ->
    let yield' a r = yield a r next
        go a = yield' a $ go $ adv a
    in go a0

-- | Loop forever without yielding (interesting) values.
forever :: LoopT m ()
{-# INLINE forever #-}
forever = iterate () id

-- | Standard @for@ loop.
for
    :: a            -- ^ Starting value of iterator
    -> (a -> Bool)  -- ^ Termination condition. The loop will terminate the
                    -- first time this is false. The termination condition
                    -- is checked at the /start/ of each iteration.
    -> (a -> a)     -- ^ Advance the iterator
    -> LoopT m a
{-# INLINE for #-}
for a0 cond adv = iterate a0 adv >>= \a -> if cond a then return a else break_

-- | Unfold a loop from the left.
unfoldl
    :: (i -> Maybe (i, a))  -- ^ @Just (i, a)@ advances the loop, yielding an
                            -- @a@. @Nothing@ terminates the loop.
    -> i                    -- ^ Starting value
    -> LoopT m a
{-# INLINE unfoldl #-}
unfoldl unf i0 = fromJust . fmap snd <$> for (unf i0) isJust (>>= unf . fst)

while
    :: Monad m
    => m Bool
    -> LoopT m ()
while cond = do
    forever
    p <- lift cond
    unless p break_
