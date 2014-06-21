{-# LANGUAGE RankNTypes #-}

module Control.Monad.Loop.Internal
    ( LoopR(..), buildLoopR, loopT, loop, unloop
    , LoopT(..), Loop, buildLoopT, runLoopT
    , cons, continue, continue_, breaking, breaking_, unbreakable, exec_
    , iterate, forever, for, unfoldl, while
    ) where

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Category ((<<<), (>>>))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe (fromJust, isJust)
import Data.Traversable (Traversable(..))
import Prelude hiding (foldr, iterate, break)

type LoopType r m a = (a -> m r -> m r) -> m r -> m r

-- | @LoopR r m a@ represents a loop over a base type @m@ yielding
-- a value @a@ at each iteration and producing a final result @m r@. It can
-- be used as a monad transformer, but there are actually no restrictions
-- on the type @m@. If the types @m@ and @r@ are known, the loop can be
-- executed with @runLoop@, but to execute the loop, it is usually wrapped
-- in a @LoopT@ using 'loopT'.
newtype LoopR r m a = LoopR { runLoopR :: LoopType r m a }

buildLoopR :: LoopType r m a -> LoopR r m a
{-# INLINE buildLoopR #-}
buildLoopR = LoopR

instance Functor (LoopR r m) where
    {-# INLINE fmap #-}
    fmap f xs = buildLoopR $ \yield -> runLoopR xs (yield . f)

instance Applicative (LoopR r m) where
    {-# INLINE pure #-}
    pure a = buildLoopR $ \yield -> yield a
    {-# INLINE (<*>) #-}
    fs <*> as = buildLoopR $ \yield ->
        runLoopR fs (\f -> runLoopR (fmap f as) yield)

instance Monad (LoopR r m) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    as >>= f = buildLoopR $ \yield ->
        runLoopR as (\a -> runLoopR (f a) yield)

instance MonadTrans (LoopR r) where
    {-# INLINE lift #-}
    lift m = buildLoopR $ \yield next -> m >>= \a -> yield a next

instance MonadIO m => MonadIO (LoopR r m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

-- | @LoopT m a@ represents a loop over a base type @m@ that yields a value
-- @a@ at each iteration. It is merely a @LoopR r m a@ where the @r@ has
-- been @forall@'d so that the /path/ the loop takes cannot depend on the
-- return value. This enables us to write @Foldable@ and @Traversable@
-- instances when the base type @m@ is @Foldable@. As @LoopR r m a@,
-- @LoopT m a@ can be used as a monad transformer, but there though there
-- are no restrictions on the type @m@. However, this library only provides
-- functions to execute the loop if @m@ is at least 'Applicative' (for
-- 'exec_'). For any other type, you may use 'runLoopT'.
newtype LoopT m a = LoopT (forall r. LoopR r m a)

-- | @Loop@ is a pure loop, without side-effects.
type Loop = LoopT Identity

-- | Provides a @LoopT m a@ for a @LoopR r m a@ which does not depend on
-- its @r@.
loopT :: (forall r. LoopR r m a) -> LoopT m a
{-# INLINE loopT #-}
loopT = LoopT

unloop :: LoopT m a -> (forall r. LoopR r m a)
{-# INLINE unloop #-}
unloop (LoopT ll) = ll

-- | @loop@ is 'loopT' where the base type has been restricted as an aid to
-- type inference. For loops over a base monad, there are usually other
-- constraints that fix the type, but for pure loops, the compiler often
-- has trouble inferring @Identity@.
loop :: (forall r. LoopR r Identity a) -> Loop a
{-# INLINE loop #-}
loop = loopT

runLoopT :: LoopT m a -> forall r. LoopType r m a
{-# INLINE runLoopT #-}
runLoopT (LoopT ll) = runLoopR ll

buildLoopT :: (forall r. LoopType r m a) -> LoopT m a
{-# INLINE buildLoopT #-}
buildLoopT f = loopT (LoopR f)

liftLoopT :: (forall r. LoopR r m a -> LoopR r m a) -> LoopT m a -> LoopT m a
{-# INLINE liftLoopT #-}
liftLoopT f (LoopT ll) = LoopT (f ll)

instance Functor (LoopT m) where
    {-# INLINE fmap #-}
    fmap f xs = buildLoopT $ \yield -> runLoopT xs (yield . f)

instance Applicative (LoopT m) where
    {-# INLINE pure #-}
    pure a = buildLoopT $ \yield -> yield a
    {-# INLINE (<*>) #-}
    fs <*> as = buildLoopT $ \yield ->
        runLoopT fs (\f -> runLoopT (fmap f as) yield)

instance Monad (LoopT m) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    as >>= f = buildLoopT $ \yield ->
        runLoopT as (\a -> runLoopT (f a) yield)

instance MonadTrans LoopT where
    {-# INLINE lift #-}
    lift m = buildLoopT $ \yield next -> m >>= \a -> yield a next

instance MonadIO m => MonadIO (LoopT m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

instance (Applicative m, Foldable m) => Foldable (LoopT m) where
    {-# INLINE foldr #-}
    foldr f r xs = foldr (<<<) id inner r
      where
        yield a next = (f a <<<) <$> next
        inner = runLoopT xs yield (pure id)

    {-# INLINE foldl' #-}
    foldl' f r xs = foldl' (!>>>) id inner r
      where
        (!>>>) h g = h >>> (g $!)
        yield a next = (flip f a !>>>) <$> next
        inner = runLoopT xs yield (pure id)

instance (Applicative m, Foldable m) => Traversable (LoopT m) where
    {-# INLINE sequenceA #-}
    sequenceA = foldr (liftA2 (\a -> liftLoopT $ cons a)) (pure $ loopT continue_)

cons :: a -> LoopR r m a -> LoopR r m a
{-# INLINE cons #-}
cons a as = buildLoopR $ \yield -> yield a . runLoopR as yield

-- | Yield a value for this iteration of the loop and continue to the next
-- iteration.
--
-- Same as pure / return.
continue :: a -> LoopR r m a
{-# INLINE continue #-}
continue = return

-- | Skip immediately to the next iteration of the loop without yielding
-- a value.
continue_ :: LoopR r m a
{-# INLINE continue_ #-}
continue_ = buildLoopR $ \_ next -> next

-- | @breaking@ passes a continuation to its argument (the "child loop")
-- which takes a final value that the child loop will yield before
-- breaking. Control resumes after tho call to @breaking@. The
-- quantification of @r@ prevents the continuation from escaping the
-- child's scope.
breaking :: (forall r. (a -> LoopR r m b) -> LoopR r m a) -> (forall s. LoopR s m a)
{-# INLINE breaking #-}
breaking child = buildLoopR $ \yield brk ->
    let breaker a = buildLoopR $ \_ _ -> yield a brk
    in runLoopR (child breaker) yield brk

-- | @breaking_@ passes a continuation to its argument (the "child loop")
-- which breaks all the way out of the child loop. Control resumes after
-- the call to @breaking_@. The quantification of @r@ prevents the
-- continuation from escaping the child's scope.
breaking_ :: (forall r. LoopR r m b -> LoopR r m a) -> (forall s. LoopR s m a)
{-# INLINE breaking_ #-}
breaking_ child = buildLoopR $ \yield brk ->
    let breaker = buildLoopR $ \_ _ -> brk
    in runLoopR (child breaker) yield brk

-- | @unbreakable@ prevents unification of existentially quantified return
-- types between @LoopR@s. In other words, a continuation from
-- 'breaking' or 'breaking_' outside of @unbreakable@ cannot be used
-- inside.
unbreakable :: (forall r. LoopR r m a) -> (forall s. LoopR s m a)
{-# INLINE unbreakable #-}
unbreakable ll = ll

-- | Execute a loop, sequencing the effects and discarding the values.
exec_ :: Applicative m => LoopT m a -> m ()
{-# INLINE exec_ #-}
exec_ xs = runLoopT xs (\_ next -> next) (pure ())

-- | Iterate forever (or until 'break' is used).
iterate
    :: a          -- ^ Starting value of iterator
    -> (a -> a)   -- ^ Advance the iterator
    -> LoopR r m a
{-# INLINE iterate #-}
iterate = \a0 adv -> buildLoopR $ \yield _ ->
    let go a = yield a $ go $ adv a
    in go a0

-- | Loop forever without yielding (interesting) values.
forever :: LoopR r m ()
{-# INLINE forever #-}
forever = iterate () id

-- | Standard @for@ loop.
for
    :: a            -- ^ Starting value of iterator
    -> (a -> Bool)  -- ^ Termination condition. The loop will terminate the
                    -- first time this is false. The termination condition
                    -- is checked at the /start/ of each iteration.
    -> (a -> a)     -- ^ Advance the iterator
    -> LoopR r m a
{-# INLINE for #-}
for = \a0 cond adv -> buildLoopR $ \yield next ->
    let go a | cond a = yield a $ go $ adv a
             | otherwise = next
    in go a0

-- | Unfold a loop from the left.
unfoldl
    :: (i -> Maybe (i, a))  -- ^ @Just (i, a)@ advances the loop, yielding an
                            -- @a@. @Nothing@ terminates the loop.
    -> i                    -- ^ Starting value
    -> LoopR r m a
{-# INLINE unfoldl #-}
unfoldl = \unf i0 ->
    fromJust . fmap snd <$> for (unf i0) isJust (>>= unf . fst)

while
    :: Monad m
    => m Bool
    -> LoopR r m ()
{-# INLINE while #-}
while = \cond -> breaking_ $ \break_ -> do
    forever
    p <- lift cond
    unless p break_
