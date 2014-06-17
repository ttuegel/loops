{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Data.Unroll
    ( Nat, N1, N2, N4, N8
    , Unroll(..), unroll1, unroll2, unroll4, unroll8
    , Unrolling(..)
    ) where

#if __GLASGOW_HASKELL__ >= 708
import GHC.TypeLits
#endif

-- Unrolling by induction
-- Can't use GHC's type level literals here, they don't support induction

data INat = S !INat | Z
data IUnroll (n :: INat) = IUnroll

predUnroll :: IUnroll (S n) -> IUnroll n
predUnroll IUnroll = IUnroll

class IUnrolling (n :: INat) where
    iUnrollFor
        :: IUnroll n
        -> a -> (a -> Bool) -> (a -> a)  -- for parameters
        -> (a -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT

    iUnrollIterate
        :: IUnroll n  -- unrolling factor
        -> a -> (a -> a)  -- iterate parameters
        -> (a -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT

instance IUnrolling Z where
    {-# INLINE iUnrollFor #-}
    iUnrollFor IUnroll = \a _ _ _ next _ -> next a

    {-# INLINE iUnrollIterate #-}
    iUnrollIterate IUnroll = \a _ _ next _ -> next a

instance IUnrolling n => IUnrolling (S n) where
    {-# INLINE iUnrollFor #-}
    iUnrollFor unroll = \a cond adv yield next brk ->
        let a' = adv a
            descend
                | cond a' = iUnrollFor (predUnroll unroll) a' cond adv yield next brk
                | otherwise = brk
        in yield a descend

    {-# INLINE iUnrollIterate #-}
    iUnrollIterate unroll = \a adv yield next brk ->
        let descend = iUnrollIterate (predUnroll unroll) (adv a) adv yield next brk
        in yield a descend

-- Unrolling using type level literals
-- Just a clean wrapper around the inductive code above

#if __GLASGOW_HASKELL__ < 708
type Nat = INat
type N1 = S Z
type N2 = S N1
type N4 = S (S N2)
type N8 = S (S (S (S N4)))
#else
type N1 = 1
type N2 = 2
type N4 = 3
type N8 = 4
#endif

-- | Proxy type for natural numbers. @n@ is the number of times the loop
-- will be unrolled into its own body. @n@ must be at least @1@, or the
-- loop would have no body at all!
#if __GLASGOW_HASKELL__ >= 708
data Unroll (n :: Nat) = Unroll
#else
data Unroll (n :: INat) = Unroll
#endif

#if __GLASGOW_HASKELL__ >= 708
type family UnLit (n :: Nat) :: INat where
    UnLit 1 = S Z
    UnLit n = S (UnLit ((-) n 1))
#else
type UnLit (n :: INat) = n
#endif

unlit :: Unroll n -> IUnroll (UnLit n)
unlit Unroll = IUnroll

-- | Do not unroll the loop at all.
unroll1 :: Unroll N1
unroll1 = Unroll

unroll2 :: Unroll N2
unroll2 = Unroll

unroll4 :: Unroll N4
unroll4 = Unroll

unroll8 :: Unroll N8
unroll8 = Unroll

#if __GLASGOW_HASKELL__ >= 708
class IUnrolling (UnLit n) => Unrolling (n :: Nat) where
#else
class IUnrolling (UnLit n) => Unrolling (n :: INat) where
#endif
    unrollFor
        :: Unroll n
        -> a -> (a -> Bool) -> (a -> a)  -- for parameters
        -> (a -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT
    {-# INLINE unrollFor #-}
    unrollFor = iUnrollFor . unlit

    unrollIterate
        :: Unroll n  -- unrolling factor
        -> a -> (a -> a)  -- iterate parameters
        -> (a -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT
    {-# INLINE unrollIterate #-}
    unrollIterate = iUnrollIterate . unlit

instance IUnrolling (UnLit n) => Unrolling n
