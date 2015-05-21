{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Loop where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), Alternative(..))
#else
import Control.Applicative (Alternative(..))
#endif
import Control.Monad (MonadPlus(..))
import Data.Foldable (Foldable(foldr, foldl', foldl))
import Data.Functor.Identity
import GHC.Types (SPEC(..))
import Prelude hiding (foldr, foldl)

data Step s a = Done | Skip s | Yield a s

-- | Promoted data type for tracking loop shapes.
data LS = F        -- ^ flat loop
        | M LS     -- ^ mapped loop
        | P        -- ^ pure loop
        | A LS LS  -- ^ applied loop
        | B LS     -- ^ bound loop
        | S LS LS  -- ^ sequential loops
        | Z        -- ^ empty loop

-- | Loops indexed by their shapes. Note that inner loops always have
-- a smaller shape than their containing loops.
data LoopI :: LS -> (* -> *) -> * -> * where
    Flat :: (s -> m (Step s a)) -> s -> LoopI 'F m a
    Map :: (a -> b) -> LoopI i m a -> LoopI ('M i) m b
    Pure :: a -> LoopI 'P m a
    Ap :: LoopI i m (a -> b) -> LoopI j m a -> LoopI ('A i j) m b
    Bind :: LoopI i m a -> (a -> Loop m b) -> LoopI ('B i) m b
    Alt :: LoopI i m a -> LoopI j m a -> LoopI ('S i j) m a
    Zero :: LoopI 'Z m a

-- | Folds over loops indexed by their shapes. The 'LS' index is used to
-- drive inlining. Because inner loops always have a smaller shape than their
-- containing loops, it is safe to call these class members recursively within
-- their own implementations: the 'LS' index of the inner loop always differs,
-- so GHC will inline all the \"recursive\" calls.
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

instance Unroll 'P where
    {-# INLINE nfoldlM #-}
    nfoldlM = \f z (Pure a) -> f z a

    {-# INLINE nfoldlM' #-}
    nfoldlM' = \f z (Pure a) -> f z a

    {-# INLINE nfoldrM #-}
    nfoldrM = \f z (Pure a) -> f a z

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

instance Unroll i => Unroll ('B i) where
    {-# INLINE nfoldlM #-}
    nfoldlM = \f z (Bind as g) ->
        nfoldlM (\y a -> foldlM f y (g a)) z as

    {-# INLINE nfoldlM' #-}
    nfoldlM' = \f z (Bind as g) ->
        nfoldlM' (\ !y a -> foldlM' f y (g a)) z as

    {-# INLINE nfoldrM #-}
    nfoldrM = \f z (Bind as g) ->
        nfoldrM (\a y -> foldrM f y (g a)) z as

instance (Unroll i, Unroll j) => Unroll ('S i j) where
    {-# INLINE nfoldlM #-}
    nfoldlM = \f z (Alt l r) -> nfoldlM f z l >>= (\y -> nfoldlM f y r)

    {-# INLINE nfoldlM' #-}
    nfoldlM' = \f z (Alt l r) -> nfoldlM' f z l >>= (\y -> nfoldlM' f y r)

    {-# INLINE nfoldrM #-}
    nfoldrM = \f z (Alt l r) -> (\y -> nfoldrM f y l) =<< (nfoldrM f z r)

instance Unroll 'Z where
    {-# INLINE nfoldlM #-}
    nfoldlM = \_ z Zero -> return z

    {-# INLINE nfoldlM' #-}
    nfoldlM' = \_ z Zero -> return z

    {-# INLINE nfoldrM #-}
    nfoldrM = \_ z Zero -> return z

data Loop m a = forall (n :: LS). Unroll n => Loop (LoopI n m a)

instance Functor m => Functor (Loop m) where
    {-# INLINE fmap #-}
    fmap = \f (Loop l) -> Loop (Map f l)

instance Applicative m => Applicative (Loop m) where
    {-# INLINE pure #-}
    pure = \a -> Loop (Pure a)

    {-# INLINE (<*>) #-}
    (<*>) = \(Loop l) (Loop r) -> Loop (Ap l r)

instance Applicative f => Alternative (Loop f) where
    {-# INLINE empty #-}
    empty = Loop Zero

    {-# INLINE (<|>) #-}
    (<|>) = \(Loop l) (Loop r) -> Loop (Alt l r)

instance Monad m => Monad (Loop m) where
    {-# INLINE return #-}
    return = \a -> Loop (Pure a)

    {-# INLINE (>>=) #-}
    (>>=) = \(Loop as) f -> Loop (Bind as f)

instance Monad m => MonadPlus (Loop m) where
    {-# INLINE mzero #-}
    mzero = Loop Zero

    {-# INLINE mplus #-}
    mplus = \(Loop l) (Loop r) -> Loop (Alt l r)

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

foldlM :: Monad m => (a -> b -> m a) -> a -> Loop m b -> m a
{-# INLINE foldlM #-}
foldlM = \f z (Loop bs) -> nfoldlM f z bs

foldlM' :: Monad m => (a -> b -> m a) -> a -> Loop m b -> m a
{-# INLINE foldlM' #-}
foldlM' = \f z (Loop bs) -> nfoldlM' f z bs

foldrM :: Monad m => (a -> b -> m b) -> b -> Loop m a -> m b
{-# INLINE foldrM #-}
foldrM = \f z (Loop bs) -> nfoldrM f z bs

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
