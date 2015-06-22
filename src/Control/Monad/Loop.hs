{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Loop where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), Alternative(..))
#else
import Control.Applicative (Alternative(..))
#endif
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Foldable (Foldable(foldr, foldl', foldl))
import Data.Functor.Identity
import GHC.Types (SPEC(..))
import Prelude hiding (foldr, foldl)

data Step s a = Done | Skip s | Yield a s

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = \f -> \case
      Yield a s -> Yield (f a) s
      Skip s -> Skip s
      Done -> Done

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
    Map :: (forall s. Step s a -> m (Step s b)) -> LoopI i m a -> LoopI ('M i) m b
    Pure :: m a -> LoopI 'P m a
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
    iterl :: Monad m => LoopI n m a -> (forall s. r -> Step s a -> m (Step s r)) -> r -> m r
    iterl' :: Monad m => LoopI n m a -> (forall s. r -> Step s a -> m (Step s r)) -> r -> m r
    iterr :: Monad m => LoopI n m a -> (forall s. Step s a -> r -> m (Step s r)) -> r -> m r

instance Unroll 'F where
    {-# INLINE iterl #-}
    iterl (Flat step s) cont z = iterl_F_loop SPEC s z where
      iterl_F_loop !spec t y = do
          a <- step t
          q <- cont y a
          case q of
            Yield x u -> iterl_F_loop spec u x
            Skip u -> iterl_F_loop spec u y
            Done -> return y

    {-# INLINE iterl' #-}
    iterl' (Flat step s) cont z = iterl'_F_loop SPEC s z where
      iterl'_F_loop !spec t !y = do
          a <- step t
          q <- cont y a
          case q of
            Yield !x u -> iterl'_F_loop spec u x
            Skip u -> iterl'_F_loop spec u y
            Done -> return y

    {-# INLINE iterr #-}
    iterr (Flat step s) cont = iterr_F_loop SPEC s where
      iterr_F_loop !spec t = \y -> do
          a <- step t
          q <- cont a y
          case q of
            Yield z u -> iterr_F_loop spec u z
            Skip u -> iterr_F_loop spec u y
            Done -> return y

instance Unroll i => Unroll ('M i) where
    {-# INLINE iterl #-}
    iterl
      (Map m (i :: LoopI i m b) :: LoopI ('M i) m a)
      (cont :: forall s. r -> Step s a -> m (Step s r))
      z =
          iterl i iterl_M_go z where
            iterl_M_go :: forall s. r -> Step s b -> m (Step s r)
            iterl_M_go r a = m a >>= cont r

    {-# INLINE iterl' #-}
    iterl'
      (Map m (i :: LoopI i m b) :: LoopI ('M i) m a)
      (cont :: forall s. r -> Step s a -> m (Step s r))
      z =
          iterl' i iterl'_M_go z where
            iterl'_M_go :: forall s. r -> Step s b -> m (Step s r)
            iterl'_M_go !r a = m a >>= cont r

    {-# INLINE iterr #-}
    iterr
      (Map m (i :: LoopI i m b) :: LoopI ('M i) m a)
      (cont :: forall s. Step s a -> r -> m (Step s r))
      z =
          iterr i iterr_M_go z where
            iterr_M_go :: forall s. Step s b -> r -> m (Step s r)
            iterr_M_go a r = do
                b <- m a
                cont b r

instance Unroll 'P where
    {-# INLINE iterl #-}
    iterl (Pure ma) cont y = do
        a <- ma
        r <- cont y (Yield a ())
        case r of
          Yield z () -> return z
          Skip () -> return y
          Done -> return y

    {-# INLINE iterl' #-}
    iterl' (Pure ma) cont y = do
        a <- ma
        r <- cont y (Yield a ())
        case r of
          Yield z () -> return z
          Skip () -> return y
          Done -> return y

    {-# INLINE iterr #-}
    iterr (Pure ma) cont y = do
        a <- ma
        r <- cont (Yield a ()) y
        case r of
          Yield z () -> return z
          Skip () -> return y
          Done -> return y

instance (Unroll i, Unroll j) => Unroll ('A i j) where
    {-# INLINE iterl #-}
    iterl
      (Ap (fs :: LoopI i m (a -> b)) (as :: LoopI j m a) :: LoopI ('A i j) m b)
      (cont :: forall s. r -> Step s b -> m (Step s r))
      z =
          iterl fs iterl_A_go z where
            iterl_A_go :: forall s. r -> Step s (a -> b) -> m (Step s r)
            iterl_A_go y a =
                case a of
                  Yield f s -> do
                      x <- iterl (Map (return . fmap f) as) cont y
                      return (Yield x s)
                  Skip s -> return (Skip s)
                  Done -> return Done

    {-# INLINE iterl' #-}
    iterl'
      (Ap (fs :: LoopI i m (a -> b)) (as :: LoopI j m a) :: LoopI ('A i j) m b)
      (cont :: forall s. r -> Step s b -> m (Step s r))
      z =
          iterl' fs iterl'_A_go z where
            iterl'_A_go :: forall s. r -> Step s (a -> b) -> m (Step s r)
            iterl'_A_go !y a =
              case a of
                Yield f s -> do
                  !x <- iterl' (Map (return . fmap f) as) cont y
                  return (Yield x s)
                Skip s -> return $ Skip s
                Done -> return Done

    {-# INLINE iterr #-}
    iterr
      (Ap (fs :: LoopI i m (a -> b)) (as :: LoopI j m a) :: LoopI ('A i j) m b)
      (cont :: forall s. Step s b -> r -> m (Step s r))
      z =
          iterr fs iterr_A_go z where
            iterr_A_go :: forall s. Step s (a -> b) -> r -> m (Step s r)
            iterr_A_go a y =
              case a of
                Yield f s -> do
                  x <- iterr (Map (return . fmap f) as) cont y
                  return (Yield x s)
                Skip s -> return (Skip s)
                Done -> return Done

instance Unroll i => Unroll ('B i) where
    {-# INLINE iterl #-}
    iterl
      (Bind (as :: LoopI i m a) (f :: a -> Loop m b) :: LoopI ('B i) m b)
      (cont :: forall s. r -> Step s b -> m (Step s r))
      z =
          iterl as iterl_B_go z where
            iterl_B_go :: forall s. r -> Step s a -> m (Step s r)
            iterl_B_go y st =
              case st of
                Yield a s ->
                  case f a of
                    Loop l -> do
                      x <- iterl l cont y
                      return (Yield x s)
                Skip s -> return (Skip s)
                Done -> return Done

    {-# INLINE iterl' #-}
    iterl'
      (Bind (as :: LoopI i m a) (f :: a -> Loop m b) :: LoopI ('B i) m b)
      (cont :: forall s. r -> Step s b -> m (Step s r))
      z =
          iterl' as iterl'_B_go z where
            iterl'_B_go :: forall s. r -> Step s a -> m (Step s r)
            iterl'_B_go y st =
              case st of
                Yield a s ->
                  case f a of
                    Loop l -> do
                      !x <- iterl l cont y
                      return (Yield x s)
                Skip s -> return (Skip s)
                Done -> return Done

    {-# INLINE iterr #-}
    iterr
      (Bind (as :: LoopI i m a) (f :: a -> Loop m b) :: LoopI ('B i) m b)
      (cont :: forall s. Step s b -> r -> m (Step s r))
      z =
          iterr as iterr_B_go z where
            iterr_B_go :: forall s. Step s a -> r -> m (Step s r)
            iterr_B_go st y =
              case st of
                Yield a s ->
                  case f a of
                    Loop l -> do
                      x <- iterr l cont y
                      return (Yield x s)
                Skip s -> return (Skip s)
                Done -> return Done

instance (Unroll i, Unroll j) => Unroll ('S i j) where
    {-# INLINE iterl #-}
    iterl (Alt i j) cont z = iterl i cont z >>= iterl j cont

    {-# INLINE iterl' #-}
    iterl' (Alt i j) cont z = iterl' i cont z >>= iterl' j cont

    {-# INLINE iterr #-}
    iterr (Alt i j) cont z = iterr i cont =<< iterr j cont z

instance Unroll 'Z where
    {-# INLINE iterl #-}
    iterl Zero cont y = do
        r <- cont y Done
        case r of
          Yield z _ -> return z
          Skip _ -> return y
          Done -> return y

    {-# INLINE iterl' #-}
    iterl' Zero cont y = do
        r <- cont y Done
        case r of
          Yield z _ -> return z
          Skip _ -> return y
          Done -> return y

    {-# INLINE iterr #-}
    iterr Zero cont y = do
        r <- cont Done y
        case r of
          Yield z _ -> return z
          Skip _ -> return y
          Done -> return y

data Loop m a = forall (n :: LS). Unroll n => Loop (LoopI n m a)

instance Applicative m => Functor (Loop m) where
    {-# INLINE fmap #-}
    fmap = \f (Loop l) -> Loop (Map (pure . fmap f) l)

instance Applicative m => Applicative (Loop m) where
    {-# INLINE pure #-}
    pure = \a -> Loop (Pure (pure a))

    {-# INLINE (<*>) #-}
    (<*>) = \(Loop l) (Loop r) -> Loop (Ap l r)

-- | TODO: '(<|>)' is not actually associative, although the folds can't tell the
-- difference. With a clever unrolling trick, it could actually be associative.
instance Applicative f => Alternative (Loop f) where
    {-# INLINE empty #-}
    empty = Loop Zero

    {-# INLINE (<|>) #-}
    (<|>) = \(Loop l) (Loop r) -> Loop (Alt l r)

instance Monad m => Monad (Loop m) where
    {-# INLINE return #-}
    return = \a -> Loop (Pure (return a))

    {-# INLINE (>>=) #-}
    (>>=) = \(Loop as) f -> Loop (Bind as f)

-- | TODO: 'mplus' is not actually associative, although the folds can't tell the
-- difference. With a clever unrolling trick, it could actually be associative.
instance Monad m => MonadPlus (Loop m) where
    {-# INLINE mzero #-}
    mzero = Loop Zero

    {-# INLINE mplus #-}
    mplus = \(Loop l) (Loop r) -> Loop (Alt l r)

instance MonadTrans Loop where
    {-# INLINE lift #-}
    lift = \inner -> Loop (Pure inner)

instance MonadIO m => MonadIO (Loop m) where
    {-# INLINE liftIO #-}
    liftIO = \inner -> lift (liftIO inner)

instance (m ~ Identity) => Foldable (Loop m) where
    {-# INLINE foldr #-}
    foldr f z as = runIdentity (foldrM foldr_go z as) where
      foldr_go a y = pure (f a y)

    {-# INLINE foldl #-}
    foldl f z bs = runIdentity (foldlM foldl_go z bs) where
      foldl_go y a = pure (f y a)

    {-# INLINE foldl' #-}
    foldl' f z bs = runIdentity (foldlM' foldl'_go z bs) where
      foldl'_go !y a = pure (f y a)

foldlM :: Monad m => (a -> b -> m a) -> a -> Loop m b -> m a
{-# INLINE foldlM #-}
foldlM f (x :: a) (Loop bs :: Loop m b) = iterl bs foldlM_go x where
  foldlM_go :: forall s. a -> Step s b -> m (Step s a)
  foldlM_go y st =
    case st of
      Yield a s -> do
        z <- f y a
        return (Yield z s)
      Skip s -> return (Skip s)
      Done -> return Done

foldlM' :: Monad m => (a -> b -> m a) -> a -> Loop m b -> m a
{-# INLINE foldlM' #-}
foldlM' f (x :: a) (Loop bs :: Loop m b) = iterl' bs foldlM'_go x where
  foldlM'_go :: forall s. a -> Step s b -> m (Step s a)
  foldlM'_go !y st =
    case st of
      Yield a s -> do
        !z <- f y a
        return (Yield z s)
      Skip s -> return (Skip s)
      Done -> return Done

foldrM :: Monad m => (a -> b -> m b) -> b -> Loop m a -> m b
{-# INLINE foldrM #-}
foldrM f (x :: b) (Loop as :: Loop m a) = iterr as foldrM_go x where
  foldrM_go :: forall s. Step s a -> b -> m (Step s b)
  foldrM_go st y =
    case st of
      Yield a s -> do
        z <- f a y
        return (Yield z s)
      Skip s -> return (Skip s)
      Done -> return Done

unfoldrM :: Functor f => (s -> f (Maybe (a, s))) -> s -> Loop f a
{-# INLINE unfoldrM #-}
unfoldrM step s = Loop (Flat (fmap maybeToStep . step) s)

maybeToStep :: Maybe (a, s) -> Step s a
{-# INLINE maybeToStep #-}
maybeToStep mst =
    case mst of
      Just (a, s) -> Yield a s
      Nothing -> Done

enumFromStepN :: (Applicative f, Num a) => a -> a -> Int -> Loop f a
{-# INLINE enumFromStepN #-}
enumFromStepN !x !y !n = Loop (Flat step (x, n)) where
  step (w, m)
    | m > 0 = pure (Yield w (w + y, m - 1))
    | otherwise = pure Done

filterM :: Monad m => (a -> m Bool) -> Loop m a -> Loop m a
{-# INLINE filterM #-}
filterM check (Loop l :: Loop m a) = Loop (Map filterM_go l) where
  filterM_go :: forall s. Step s a -> m (Step s a)
  filterM_go st =
    case st of
      Yield a s -> do
        p <- check a
        return (if p then Yield a s else Skip s)
      Skip s -> return (Skip s)
      Done -> return Done

filter :: Monad m => (a -> Bool) -> Loop m a -> Loop m a
{-# INLINE filter #-}
filter check as = filterM (return . check) as

generateM :: Monad m => Int -> (Int -> m a) -> Loop m a
{-# INLINE generateM #-}
generateM !len gen = Loop (Flat step 0) where
  step n
    | n < len = do
        a <- gen n
        return $ Yield a (n + 1)
    | otherwise = return Done

generate :: Monad m => Int -> (Int -> a) -> Loop m a
{-# INLINE generate #-}
generate len gen = generateM len (return . gen)
