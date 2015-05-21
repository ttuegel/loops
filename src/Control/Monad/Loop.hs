{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Loop where

import Data.Foldable
import Data.Functor.Identity
import GHC.Types (SPEC(..))

-- | Loop shapes, flat or nested.
data LS = F | M LS | A LS LS | B LS LS

data Step s a = Done | Skip s | Yield a s

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = \f r ->
        case r of
          Done -> Done
          Skip s -> Skip s
          Yield a s -> Yield (f a) s

data LoopI :: LS -> (* -> *) -> * -> * where
    Flat :: (s -> m (Step s a)) -> s -> LoopI 'F m a
    Map :: (a -> b) -> LoopI i m a -> LoopI ('M i) m b
    Ap :: LoopI i m (a -> b) -> LoopI j m a -> LoopI ('A i j) m b
    Bind :: LoopI i m a -> (a -> LoopI j m b) -> LoopI ('B i j) m b

class Unroll (n :: LS) where
    nfoldlM :: Monad m => (a -> b -> m a) -> a -> LoopI n m b -> m a
    nfoldlM' :: Monad m => (a -> b -> m a) -> a -> LoopI n m b -> m a
    nfoldrM :: Monad m => (a -> b -> m b) -> b -> LoopI n m a -> m b

instance Unroll 'F where
    {-# INLINE nfoldlM #-}
    nfoldlM = \f z (Flat step s) ->
        let nfoldlM_F_loop !spec t y = do
                r <- step t
                case r of
                  Yield a t' -> f y a >>= nfoldlM_F_loop spec t'
                  Skip t' -> nfoldlM_F_loop spec t' y
                  Done -> return y
        in nfoldlM_F_loop SPEC s z

    {-# INLINE nfoldlM' #-}
    nfoldlM' = \f z (Flat step s) ->
        let nfoldlM'_F_loop !spec t !y = do
                r <- step t
                case r of
                  Yield a t' -> f y a >>= nfoldlM'_F_loop spec t'
                  Skip t' -> nfoldlM'_F_loop spec t' y
                  Done -> return y
        in nfoldlM'_F_loop SPEC s z

    {-# INLINE nfoldrM #-}
    nfoldrM = \f z (Flat step s) ->
        let nfoldrM_F_loop !spec t = do
                r <- step t
                case r of
                  Yield a t' -> f a =<< nfoldrM_F_loop spec t'
                  Skip t' -> nfoldrM_F_loop spec t'
                  Done -> return z
        in nfoldrM_F_loop SPEC s

instance Unroll i => Unroll ('M i) where
    {-# INLINE nfoldlM #-}
    nfoldlM = \f z (Map g l) -> nfoldlM (\y a -> f y (g a)) z l

    {-# INLINE nfoldlM' #-}
    nfoldlM' = \f z (Map g l) -> nfoldlM' (\ !y a -> f y (g a)) z l

    {-# INLINE nfoldrM #-}
    nfoldrM = \f z (Map g l) -> nfoldrM (\a y -> f (g a) y) z l

instance (Unroll i, Unroll j) => Unroll ('A i j) where
    {-# INLINE nfoldlM #-}
    nfoldlM = \f z (Ap gs as) ->
        nfoldlM (\y g -> nfoldlM (\x a -> f x (g a)) y as) z gs

    {-# INLINE nfoldlM' #-}
    nfoldlM' = \f z (Ap gs as) ->
        nfoldlM' (\ !y g -> nfoldlM' (\ !x a -> f x (g a)) y as) z gs

    {-# INLINE nfoldrM #-}
    nfoldrM = \f z (Ap gs as) ->
        nfoldrM (\g y -> nfoldrM (\a x -> f (g a) x) y as) z gs

instance (Unroll i, Unroll j) => Unroll ('B i j) where
    {-# INLINE nfoldlM #-}
    nfoldlM = \f z (Bind as g) ->
        nfoldlM (\y a -> nfoldlM f y (g a)) z as

    {-# INLINE nfoldlM' #-}
    nfoldlM' = \f z (Bind as g) ->
        nfoldlM' (\ !y a -> nfoldlM' f y (g a)) z as

    {-# INLINE nfoldrM #-}
    nfoldrM = \f z (Bind as g) ->
        nfoldrM (\a y -> nfoldrM f y (g a)) z as

data Loop m a = forall (n :: LS). Unroll n => Loop (LoopI n m a)

instance Functor m => Functor (Loop m) where
    {-# INLINE fmap #-}
    fmap = \f (Loop l) -> Loop (Map f l)

instance Applicative m => Applicative (Loop m) where
    {-# INLINE pure #-}
    pure = \a ->
      let pure_step !_ = \case
              True -> pure (Yield a False)
              False -> pure Done
      in Loop (Flat (pure_step SPEC) True)

    {-# INLINE (<*>) #-}
    (<*>) = \(Loop l) (Loop r) -> Loop (Ap l r)

instance Foldable (Loop Identity) where
    {-# INLINE foldr #-}
    foldr = \f z (Loop l) ->
        let mf = \a y -> Identity (f a y) in runIdentity (nfoldrM mf z l)

    {-# INLINE foldl #-}
    foldl = \f z (Loop l) ->
        let mf = \y a -> Identity (f y a) in runIdentity (nfoldlM mf z l)

    {-# INLINE foldl' #-}
    foldl' = \f z (Loop l) ->
        let mf = \ !y a -> Identity (f y a) in runIdentity (nfoldlM' mf z l)

unfoldrM :: Functor f => (s -> f (Maybe (a, s))) -> s -> Loop f a
{-# INLINE unfoldrM #-}
unfoldrM = \step s -> Loop (Flat (fmap maybeToStep . step) s)

maybeToStep :: Maybe (a, s) -> Step s a
{-# INLINE maybeToStep #-}
maybeToStep = \case
  Just (a, s) -> Yield a s
  Nothing -> Done

enumFromStepN :: (Applicative f, Num a) => a -> a -> Int -> Loop f a
{-# INLINE enumFromStepN #-}
enumFromStepN = \ !x !y !n ->
    let step (w, m)
          | m > 0 = pure (Yield w (w + y, m - 1))
          | otherwise = pure Done
    in Loop (Flat step (x, n))
