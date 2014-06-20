{-# LANGUAGE RankNTypes #-}

module Control.Monad.Loop.Internal
    ( LoopLike(..), buildLoopLike, loopT, loop, unloop
    , LoopT(..), Loop, buildLoopT, runLoopT
    , exec, exec_
    , cons, continue, continue_, breaking, breaking_, unbreakable
    , iterate, forever, for, unfoldl, while
    , module Data.Unroll
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

import Data.Unroll

type LoopType r m a = (a -> m r -> m r) -> m r -> m r

-- | @LoopLike r m a@ represents a loop over a base type @m@ yielding
-- a value @a@ at each iteration and producing a final result @m r@. It can
-- be used as a monad transformer, but there are actually no restrictions
-- on the type @m@. If the types @m@ and @r@ are known, the loop can be
-- executed with @runLoopLike@, but to execute the loop, it is usually wrapped
-- in a @LoopT@ using 'loopT'.
newtype LoopLike r m a = LoopLike { runLoopLike :: LoopType r m a }

buildLoopLike :: LoopType r m a -> LoopLike r m a
{-# INLINE buildLoopLike #-}
buildLoopLike = LoopLike

instance Functor (LoopLike r m) where
    {-# INLINE fmap #-}
    fmap f xs = buildLoopLike $ \yield -> runLoopLike xs (yield . f)

instance Applicative (LoopLike r m) where
    {-# INLINE pure #-}
    pure a = buildLoopLike $ \yield -> yield a
    {-# INLINE (<*>) #-}
    fs <*> as = buildLoopLike $ \yield ->
        runLoopLike fs (\f -> runLoopLike (fmap f as) yield)

instance Monad (LoopLike r m) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    as >>= f = buildLoopLike $ \yield ->
        runLoopLike as (\a -> runLoopLike (f a) yield)

instance MonadTrans (LoopLike r) where
    {-# INLINE lift #-}
    lift m = buildLoopLike $ \yield next -> m >>= \a -> yield a next

instance MonadIO m => MonadIO (LoopLike r m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

-- | @LoopT m a@ represents a loop over a base type @m@ that yields a value
-- @a@ at each iteration. It is merely a @LoopLike r m a@ where the @r@ has
-- been @forall@'d so that the /path/ the loop takes cannot depend on the
-- return value. This enables us to write @Foldable@ and @Traversable@
-- instances when the base type @m@ is @Foldable@. As @LoopLike r m a@,
-- @LoopT m a@ can be used as a monad transformer, but there though there
-- are no restrictions on the type @m@. However, this library only provides
-- functions to execute the loop if @m@ is at least 'Applicative' (for
-- 'exec_'). For any other type, you may use 'runLoopT'.
newtype LoopT m a = LoopT (forall r. LoopLike r m a)

-- | @Loop@ is a pure loop, without side-effects.
type Loop = LoopT Identity

-- | Provides a @LoopT m a@ for a @LoopLike r m a@ which does not depend on
-- its @r@.
loopT :: (forall r. LoopLike r m a) -> LoopT m a
{-# INLINE loopT #-}
loopT = LoopT

unloop :: LoopT m a -> (forall r. LoopLike r m a)
{-# INLINE unloop #-}
unloop (LoopT ll) = ll

-- | @loop@ is 'loopT' where the base type has been restricted as an aid to
-- type inference. For loops over a base monad, there are usually other
-- constraints that fix the type, but for pure loops, the compiler often
-- has trouble inferring @Identity@.
loop :: (forall r. LoopLike r Identity a) -> Loop a
{-# INLINE loop #-}
loop = loopT

runLoopT :: LoopT m a -> forall r. LoopType r m a
{-# INLINE runLoopT #-}
runLoopT (LoopT ll) = runLoopLike ll

buildLoopT :: (forall r. LoopType r m a) -> LoopT m a
{-# INLINE buildLoopT #-}
buildLoopT f = loopT (LoopLike f)

liftLoopT :: (forall r. LoopLike r m a -> LoopLike r m a) -> LoopT m a -> LoopT m a
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

cons :: a -> LoopLike r m a -> LoopLike r m a
{-# INLINE cons #-}
cons a as = buildLoopLike $ \yield -> yield a . runLoopLike as yield

-- | Yield a value for this iteration of the loop and continue to the next
-- iteration.
--
-- Same as pure / return.
continue :: a -> LoopLike r m a
{-# INLINE continue #-}
continue = return

-- | Skip immediately to the next iteration of the loop without yielding
-- a value.
continue_ :: LoopLike r m a
{-# INLINE continue_ #-}
continue_ = buildLoopLike $ \_ next -> next

-- | @breaking@ passes a continuation to its argument (the "child loop")
-- which takes a final value that the child loop will yield before
-- breaking. Control resumes after tho call to @breaking@. The
-- quantification of @r@ prevents the continuation from escaping the
-- child's scope.
breaking :: (forall r. (a -> LoopLike r m b) -> LoopLike r m a) -> (forall s. LoopLike s m a)
{-# INLINE breaking #-}
breaking child = buildLoopLike $ \yield brk ->
    let breaker a = buildLoopLike $ \_ _ -> yield a brk
    in runLoopLike (child breaker) yield brk

-- | @breaking_@ passes a continuation to its argument (the "child loop")
-- which breaks all the way out of the child loop. Control resumes after
-- the call to @breaking_@. The quantification of @r@ prevents the
-- continuation from escaping the child's scope.
breaking_ :: (forall r. LoopLike r m b -> LoopLike r m a) -> (forall s. LoopLike s m a)
{-# INLINE breaking_ #-}
breaking_ child = buildLoopLike $ \yield brk ->
    let breaker = buildLoopLike $ \_ _ -> brk
    in runLoopLike (child breaker) yield brk

-- | @unbreakable@ prevents unification of existentially quantified return
-- types between @LoopLikes@. In other words, a continuation from
-- 'breaking' or 'breaking_' outside of @unbreakable@ cannot be used
-- inside.
unbreakable :: (forall r. LoopLike r m a) -> (forall s. LoopLike s m a)
{-# INLINE unbreakable #-}
unbreakable ll = ll

-- | Execute a loop, sequencing the effects and discarding the values.
exec_ :: Applicative m => LoopT m a -> m ()
{-# INLINE exec_ #-}
exec_ xs = runLoopT xs (\_ next -> next) (pure ())

exec :: Applicative m => LoopT m a -> m (Maybe a)
{-# INLINE exec #-}
exec xs = runLoopT xs (\a -> fmap (yielding a)) (pure Nothing)
  where
    yielding a Nothing = Just a
    yielding _ x@(Just _) = x

-- | Iterate forever (or until 'break' is used).
iterate
    :: Unrolling n
    => Unroll n   -- ^ Unrolling factor
    -> a          -- ^ Starting value of iterator
    -> (a -> a)   -- ^ Advance the iterator
    -> LoopLike r m a
{-# INLINE iterate #-}
iterate unr = \a0 adv -> buildLoopLike $ \yield _ ->
    let go a = unroll unr a adv yield go
    in go a0

-- | Loop forever without yielding (interesting) values.
forever :: Unrolling n => Unroll n -> LoopLike r m ()
{-# INLINE forever #-}
forever unr = iterate unr () id

-- | Standard @for@ loop.
for
    :: Unrolling n
    => Unroll n     -- ^ Unrolling factor
    -> a            -- ^ Starting value of iterator
    -> (a -> Bool)  -- ^ Termination condition. The loop will terminate the
                    -- first time this is false. The termination condition
                    -- is checked at the /start/ of each iteration.
    -> (a -> a)     -- ^ Advance the iterator
    -> LoopLike r m a
{-# INLINE for #-}
for unr = \a0 cond adv ->
    -- For some reason, checking cond a0 twice tricks GHC into behaving,
    -- even though the generated Core looks the same.
    if cond a0
      then breaking_ $ \break_ -> do
          a <- iterate unr a0 adv
          if cond a then return a else break_
      else continue_

-- | Unfold a loop from the left.
unfoldl
    :: Unrolling n
    => Unroll n             -- ^ Unrolling factor
    -> (i -> Maybe (i, a))  -- ^ @Just (i, a)@ advances the loop, yielding an
                            -- @a@. @Nothing@ terminates the loop.
    -> i                    -- ^ Starting value
    -> LoopLike r m a
{-# INLINE unfoldl #-}
unfoldl unr = \unf i0 ->
    fromJust . fmap snd <$> for unr (unf i0) isJust (>>= unf . fst)

while
    :: (Unrolling n, Monad m)
    => Unroll n
    -> m Bool
    -> LoopLike r m ()
{-# INLINE while #-}
while unr = \cond -> breaking_ $ \break_ -> do
    forever unr
    p <- lift cond
    unless p break_
