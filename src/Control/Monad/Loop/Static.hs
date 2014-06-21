{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Control.Monad.Loop.Static
    ( Nat, N1, N2, N4, N8
    , Static(..), unroll1, unroll2, unroll4, unroll8
    , Unrolling, iterateS
    ) where

#if __GLASGOW_HASKELL__ >= 708
import GHC.TypeLits
#endif

import Control.Monad.Loop.Internal

-- Unrolling by induction
-- Can't use GHC's type level literals here, they don't support induction

data INat = S !INat | Z
data IStatic (n :: INat) = IStatic

class IUnrolling (n :: INat) where
    iIterateS
        :: IStatic n  -- unrolling factor
        -> a -> (a -> a)  -- iterate parameters
        -> (a -> m r -> m r) -> (a -> m r) -> m r  -- un-newtyped LoopT

instance IUnrolling Z where
    {-# INLINE iIterateS #-}
    iIterateS IStatic = \a _ _ next -> next a

instance IUnrolling n => IUnrolling (S n) where
    {-# INLINE iIterateS #-}
    iIterateS unr = \a adv yield next ->
        let descend = iIterateS (_pred unr) (adv a) adv yield next
        in yield a descend
      where
        _pred :: IStatic (S n) -> IStatic n
        _pred IStatic = IStatic

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
data Static (n :: Nat) = Static
#else
data Static (n :: INat) = Static

class KnownNat (n :: INat) where
    natVal :: Static n -> Integer

instance KnownNat Z where
    {-# INLINE natVal #-}
    natVal = \Static -> 0

instance KnownNat n => KnownNat (S n) where
    {-# INLINE natVal #-}
    natVal = \n -> 1 + natVal (_pred n)
      where
        _pred :: Static (S n) -> Static n
        _pred Static = Static
#endif

#if __GLASGOW_HASKELL__ >= 708
type family UnLit (n :: Nat) :: INat where
    UnLit 1 = S Z
    UnLit n = S (UnLit ((-) n 1))
#else
type UnLit (n :: INat) = n
#endif

unlit :: Static n -> IStatic (UnLit n)
unlit Static = IStatic

-- | Do not unroll the loop at all.
unroll1 :: Static N1
unroll1 = Static

unroll2 :: Static N2
unroll2 = Static

unroll4 :: Static N4
unroll4 = Static

unroll8 :: Static N8
unroll8 = Static

#if __GLASGOW_HASKELL__ >= 708
class IUnrolling (UnLit n) => Unrolling (n :: Nat)
#else
class IUnrolling (UnLit n) => Unrolling (n :: INat)
#endif

iterateS
    :: Unrolling n
    => Static n  -- unrolling factor
    -> a -> (a -> a)  -- iterate parameters
    -> LoopR r m a -- un-newtyped LoopT
{-# INLINE iterateS #-}
iterateS unr = \a0 adv -> buildLoopR $ \yield next ->
    iIterateS (unlit unr) a0 adv yield (const next)

instance IUnrolling (UnLit n) => Unrolling n
