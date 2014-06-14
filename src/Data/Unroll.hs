{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Data.Unroll where

import GHC.TypeLits

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
        -> (a -> m r -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT

    iUnrollIterate
        :: IUnroll n  -- unrolling factor
        -> a -> (a -> a)  -- iterate parameters
        -> (a -> m r -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT

instance IUnrolling Z where
    {-# INLINE iUnrollFor #-}
    iUnrollFor IUnroll a _ _ _ next _ = next a

    {-# INLINE iUnrollIterate #-}
    iUnrollIterate IUnroll a _ _ next _ = next a

instance IUnrolling n => IUnrolling (S n) where
    {-# INLINE iUnrollFor #-}
    iUnrollFor unroll a cond adv yield next brk =
        yield a descend brk
      where
        a' = adv a
        descend | cond a' = iUnrollFor (predUnroll unroll) a' cond adv yield next brk
                | otherwise = brk

    {-# INLINE iUnrollIterate #-}
    iUnrollIterate unroll a adv yield next brk =
        yield a descend brk
      where
        descend = iUnrollIterate (predUnroll unroll) (adv a) adv yield next brk

-- Unrolling using type level literals
-- Just a clean wrapper around the inductive code above

-- | Proxy type for GHC's type level literal natural numbers. @n@ is the
-- number of times the loop will be unrolled into its own body.
data Unroll (n :: Nat) = Unroll

type family UnLit (n :: Nat) :: INat where
    UnLit 1 = S Z
    UnLit n = S (UnLit ((-) n 1))

unlit :: Unroll n -> IUnroll (UnLit n)
unlit Unroll = IUnroll

-- | Do not unroll the loop at all.
noUnroll :: Unroll 1
noUnroll = Unroll

class IUnrolling (UnLit n) => Unrolling (n :: Nat) where
    unrollFor
        :: Unroll n
        -> a -> (a -> Bool) -> (a -> a)  -- for parameters
        -> (a -> m r -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT
    {-# INLINE unrollFor #-}
    unrollFor = iUnrollFor . unlit

    unrollIterate
        :: Unroll n  -- unrolling factor
        -> a -> (a -> a)  -- iterate parameters
        -> (a -> m r -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT
    {-# INLINE unrollIterate #-}
    unrollIterate = iUnrollIterate . unlit

instance IUnrolling (UnLit n) => Unrolling n
