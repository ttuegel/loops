{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Data.Unroll where

import GHC.TypeLits

-- | Proxy type for GHC's type level literal natural numbers. @n@ is the
-- number of times the loop will be unrolled into its own body.
data Unroll (n :: Nat) = Unroll

data INat = S !INat | Z
data IUnroll (n :: INat) = IUnroll

-- | Do not unroll the loop at all.
noUnroll :: Unroll 1
noUnroll = Unroll

predUnroll :: IUnroll (S n) -> IUnroll n
predUnroll IUnroll = IUnroll

type family UnLit (n :: Nat) :: INat where
    UnLit 1 = S Z
    UnLit n = S (UnLit ((-) n 1))

unlit :: Unroll n -> IUnroll (UnLit n)
unlit Unroll = IUnroll

class Unrolling (n :: INat) where
    unrollFor
        :: IUnroll n
        -> a -> (a -> Bool) -> (a -> a)  -- for parameters
        -> (a -> m r -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT

    unrollIterate
        :: IUnroll n  -- unrolling factor
        -> a -> (a -> a)  -- iterate parameters
        -> (a -> m r -> m r -> m r) -> (a -> m r) -> m r -> m r  -- un-newtyped LoopT

instance Unrolling Z where
    {-# INLINE unrollFor #-}
    unrollFor IUnroll a _ _ _ next _ = next a

    {-# INLINE unrollIterate #-}
    unrollIterate IUnroll a _ _ next _ = next a

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
