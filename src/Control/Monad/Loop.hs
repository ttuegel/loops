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
data LS = F | N LS LS

data Step s a = Done | Skip s | Yield a s

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = \f r ->
        case r of
          Done -> Done
          Skip s -> Skip s
          Yield a s -> Yield (f a) s

data NLoop :: LS -> (* -> *) -> * -> * where
    Loop0 :: (s -> m (Step s a)) -> s -> NLoop 'F m a
    Ap :: NLoop i m (a -> b) -> NLoop j m a -> NLoop ('N i j) m b

instance Functor f => Functor (NLoop i f) where
    {-# INLINE fmap #-}
    fmap = fmap_go SPEC where
      fmap_go :: Functor m => SPEC -> (a -> b) -> NLoop i m a -> NLoop i m b
      fmap_go !_ = \f -> \case
        (Loop0 step s) ->
            let step' t = fmap (fmap f) (step t)
            in Loop0 step' s
        (Ap l r) -> Ap (fmap (f .) l) r

class Unroll (n :: LS) where
    nfoldlM :: Monad m => (a -> b -> m a) -> a -> NLoop n m b -> m a
    nfoldlM' :: Monad m => (a -> b -> m a) -> a -> NLoop n m b -> m a
    nfoldrM :: Monad m => (a -> b -> m b) -> b -> NLoop n m a -> m b

instance Unroll 'F where
    {-# INLINE nfoldlM #-}
    nfoldlM = nfoldlM_Z_go where
      nfoldlM_Z_go :: Monad m => (a -> b -> m a) -> a -> NLoop 'F m b -> m a
      nfoldlM_Z_go f z (Loop0 step s) = nfoldlM_Z_loop SPEC s z where
        nfoldlM_Z_loop !spec t y = do
            r <- step t
            case r of
              Yield a t' -> f y a >>= nfoldlM_Z_loop spec t'
              Skip t' -> nfoldlM_Z_loop spec t' y
              Done -> return y

    {-# INLINE nfoldlM' #-}
    nfoldlM' = \f z (Loop0 step s) ->
        let nfoldlM'_Z_loop !spec t !y = do
                r <- step t
                case r of
                  Yield a t' -> f y a >>= nfoldlM'_Z_loop spec t'
                  Skip t' -> nfoldlM'_Z_loop spec t' y
                  Done -> return y
        in nfoldlM'_Z_loop SPEC s z

    {-# INLINE nfoldrM #-}
    nfoldrM = nfoldrM_Z_go where
      nfoldrM_Z_go :: Monad m => (a -> b -> m b) -> b -> NLoop 'F m a -> m b
      nfoldrM_Z_go f z (Loop0 step s) = nfoldrM_Z_loop SPEC s where
        nfoldrM_Z_loop !spec t = do
            r <- step t
            case r of
              Yield a t' -> f a =<< nfoldrM_Z_loop spec t'
              Skip t' -> nfoldrM_Z_loop spec t'
              Done -> return z

instance (Unroll i, Unroll j) => Unroll ('N i j) where
    {-# INLINE nfoldlM #-}
    nfoldlM = nfoldlM_S_go where
      nfoldlM_S_go :: Monad m => (a -> b -> m a) -> a -> NLoop ('N i j) m b -> m a
      nfoldlM_S_go f z (Ap l r) = nfoldlM nfoldlM_S_loop z l where
        nfoldlM_S_loop = \y g -> nfoldlM (\x a -> f x (g a)) y r

    {-# INLINE nfoldlM' #-}
    nfoldlM' = nfoldlM'_S_go where
      nfoldlM'_S_go :: Monad m => (a -> b -> m a) -> a -> NLoop ('N i j) m b -> m a
      nfoldlM'_S_go f z (Ap l r) = nfoldlM' nfoldlM'_S_loop z l where
        nfoldlM'_S_loop = \ !y g -> nfoldlM' (\ !x a -> f x (g a)) y r

    {-# INLINE nfoldrM #-}
    nfoldrM = nfoldrM_S_go where
      nfoldrM_S_go :: Monad m => (a -> b -> m b) -> b -> NLoop ('N i j) m a -> m b
      nfoldrM_S_go f z (Ap l r) = nfoldrM nfoldrM_S_loop z l where
        nfoldrM_S_loop = \g y -> nfoldrM (\a x -> f (g a) x) y r

data Loop m a = forall (n :: LS). Unroll n => Loop (NLoop n m a)

instance Functor m => Functor (Loop m) where
    {-# INLINE fmap #-}
    fmap = \f (Loop l) -> Loop (fmap f l)

instance Applicative m => Applicative (Loop m) where
    {-# INLINE pure #-}
    pure = \a ->
      let pure_step !_ = \case
              True -> pure (Yield a False)
              False -> pure Done
      in Loop (Loop0 (pure_step SPEC) True)

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
unfoldrM = \step s -> Loop (Loop0 (fmap maybeToStep . step) s)

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
    in Loop (Loop0 step (x, n))
