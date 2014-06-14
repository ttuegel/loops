{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Loop.Internal
    ( LoopT(..), Loop, loop
    , Unroll(..), UnTL, Unrolling(), noUnroll
    , cons, continue, continue_, break, break_, exec_
    , iterate, forever, for, unfoldl, while
    ) where

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Category ((<<<), (>>>))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified GHC.TypeLits as TL
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe (fromJust, isJust)
import Data.Traversable (Traversable(..))
import Prelude hiding (foldr, iterate, break)

-- | @LoopT m a@ represents a loop over a base type @m@ that yields a value
-- @a@ at each iteration. It can be used as a monad transformer, but there
-- are actually no restrictions on the type @m@. However, this library only
-- provides functions to execute the loop if @m@ is at least 'Applicative'
-- (for 'exec_'). If @m@ is also 'Foldable', so is @LoopT m@. For any other
-- type, you may use 'runLoopT'.
newtype LoopT m a = LoopT
    { runLoopT :: forall r. (a -> m r -> m r -> m r) -> m r -> m r -> m r }

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
        runLoopT fs (\f -> runLoopT (fmap f as) yield) next

instance Monad (LoopT m) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    as >>= f = LoopT $ \yield next ->
        runLoopT as (\a -> runLoopT (f a) yield) next

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

-- | Yield a value for this iteration of the loop and continue to the next
-- iteration.
--
-- Same as pure / return.
continue :: a -> LoopT m a
{-# INLINE continue #-}
continue a = LoopT $ \yield next -> yield a next

-- | Skip immediately to the next iteration of the loop without yielding
-- a value.
continue_ :: LoopT m a
{-# INLINE continue_ #-}
continue_ = LoopT $ \_ next _ -> next

-- | Yield a value for this iteration of the loop and skip all the remaining
-- iterations of the immediately-enclosing loop.
break :: a -> LoopT m a
{-# INLINE break #-}
break a = LoopT $ \yield _ brk -> yield a brk brk

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
    :: Unrolling (UnTL n)
    => Unroll n   -- ^ Unrolling factor
    -> a          -- ^ Starting value of iterator
    -> (a -> a)   -- ^ Advance the iterator
    -> LoopT m a
{-# INLINE iterate #-}
iterate unroll = \a0 adv -> LoopT $ \yield next _ ->
    let go a = unrollIterate (fromTypeLit unroll) a adv yield go next
    in go a0

-- | Loop forever without yielding (interesting) values.
forever :: Unrolling (UnTL n) => Unroll n -> LoopT m ()
{-# INLINE forever #-}
forever unroll = iterate unroll () id

-- | Standard @for@ loop.
for
    :: Unrolling (UnTL n)
    => Unroll n     -- ^ Unrolling factor
    -> a            -- ^ Starting value of iterator
    -> (a -> Bool)  -- ^ Termination condition. The loop will terminate the
                    -- first time this is false. The termination condition
                    -- is checked at the /start/ of each iteration.
    -> (a -> a)     -- ^ Advance the iterator
    -> LoopT m a
{-# INLINE for #-}
for unroll = \a0 cond adv -> LoopT $ \yield next _ ->
    let go a = unrollFor (fromTypeLit unroll) a cond adv yield go next
    in if cond a0 then go a0 else next

-- | Unfold a loop from the left.
unfoldl
    :: Unrolling (UnTL n)
    => Unroll n             -- ^ Unrolling factor
    -> (i -> Maybe (i, a))  -- ^ @Just (i, a)@ advances the loop, yielding an
                            -- @a@. @Nothing@ terminates the loop.
    -> i                    -- ^ Starting value
    -> LoopT m a
{-# INLINE unfoldl #-}
unfoldl unroll = \unf i0 ->
    fromJust . fmap snd <$> for unroll (unf i0) isJust (>>= unf . fst)

while
    :: (Unrolling (UnTL n), Monad m)
    => Unroll n
    -> m Bool
    -> LoopT m ()
{-# INLINE while #-}
while unroll = \cond -> do
    forever unroll
    p <- lift cond
    unless p break_

-- | Proxy type for GHC's type level literal natural numbers. @n@ is the
-- number of times the loop will be unrolled into its own body.
data Unroll (n :: TL.Nat) = Unroll

data Nat = S !Nat | Z
data UnrollInd (n :: Nat) = UnrollInd

-- | Do not unroll the loop at all.
noUnroll :: Unroll 1
noUnroll = Unroll

predUnroll :: UnrollInd (S n) -> UnrollInd n
predUnroll UnrollInd = UnrollInd

type family UnTL (n :: TL.Nat) :: Nat where
    UnTL 1 = S Z
    UnTL n = S (UnTL ((TL.-) n 1))

fromTypeLit :: Unroll n -> UnrollInd (UnTL n)
fromTypeLit Unroll = UnrollInd

class Unrolling (n :: Nat) where
    unrollFor
        :: UnrollInd n
        -> a -> (a -> Bool) -> (a -> a)  -- for parameters
        -> (a -> m r -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT

    unrollIterate
        :: UnrollInd n  -- unrolling factor
        -> a -> (a -> a)  -- iterate parameters
        -> (a -> m r -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT

instance Unrolling Z where
    {-# INLINE unrollFor #-}
    unrollFor UnrollInd a _ _ _ next _ = next a

    {-# INLINE unrollIterate #-}
    unrollIterate UnrollInd a _ _ next _ = next a

instance Unrolling n => Unrolling (S n) where
    {-# INLINE unrollFor #-}
    unrollFor unroll a cond adv yield next brk =
        yield a descend brk
      where
        a' = adv a
        descend | cond a' = unrollFor (predUnroll unroll) a' cond adv yield next brk
                | otherwise = brk

    {-# INLINE unrollIterate #-}
    unrollIterate unroll a adv yield next brk =
        yield a descend brk
      where
        descend = unrollIterate (predUnroll unroll) (adv a) adv yield next brk
