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
class Iter (n :: LS) where
    iterl :: Monad m => LoopI n m a -> (forall s. r -> Step s a -> m (Step s r)) -> r -> m r
    iterl' :: Monad m => LoopI n m a -> (forall s. r -> Step s a -> m (Step s r)) -> r -> m r
    iterr :: Monad m => LoopI n m a -> (forall s. Step s a -> r -> m (Step s r)) -> r -> m r

instance Iter 'F where
    {-# INLINE iterl #-}
    iterl = \(Flat step s) cont ->
        let iterl_F_loop !spec t = \y -> do
                q <- step t >>= cont y
                case q of
                  Yield z u -> iterl_F_loop spec u z
                  Skip u -> iterl_F_loop spec u y
                  Done -> return y
        in iterl_F_loop SPEC s

    {-# INLINE iterl' #-}
    iterl' = \(Flat step s) cont ->
        let iterl'_F_loop !spec t = \ !y -> do
                q <- step t >>= cont y
                case q of
                  Yield !z u -> iterl'_F_loop spec u z
                  Skip u -> iterl'_F_loop spec u y
                  Done -> return y
        in iterl'_F_loop SPEC s

    {-# INLINE iterr #-}
    iterr = \(Flat step s) cont ->
        let iterr_F_loop !spec t = \y -> do
                q <- step t >>= \a -> cont a y
                case q of
                  Yield z u -> iterr_F_loop spec u z
                  Skip u -> iterr_F_loop spec u y
                  Done -> return y
        in iterr_F_loop SPEC s

instance Iter i => Iter ('M i) where
    {-# INLINE iterl #-}
    iterl = \(Map m i) cont -> iterl i $ \r a -> m a >>= cont r

    {-# INLINE iterl' #-}
    iterl' = \(Map m i) cont -> iterl' i $ \ !r a -> m a >>= cont r

    {-# INLINE iterr #-}
    iterr = \(Map m i) cont -> iterr i $ \a r -> m a >>= \b -> cont b r

instance Iter 'P where
    {-# INLINE iterl #-}
    iterl = \(Pure ma) cont y -> do
        a <- ma
        r <- cont y (Yield a ())
        case r of
          Yield z () -> return z
          Skip () -> return y
          Done -> return y

    {-# INLINE iterl' #-}
    iterl' = \(Pure ma) cont y -> do
        a <- ma
        r <- cont y (Yield a ())
        case r of
          Yield z () -> return z
          Skip () -> return y
          Done -> return y

    {-# INLINE iterr #-}
    iterr = \(Pure ma) cont y -> do
        a <- ma
        r <- cont (Yield a ()) y
        case r of
          Yield z () -> return z
          Skip () -> return y
          Done -> return y

instance (Iter i, Iter j) => Iter ('A i j) where
    {-# INLINE iterl #-}
    iterl = \(Ap fs as) cont ->
        iterl fs $ \y -> \case
          Yield f s -> do
            z <- iterl (Map (return . fmap f) as) cont y
            return $ Yield z s
          Skip s -> return $ Skip s
          Done -> return Done

    {-# INLINE iterl' #-}
    iterl' = \(Ap fs as) cont ->
        iterl' fs $ \ !y -> \case
          Yield f s -> do
            !z <- iterl' (Map (return . fmap f) as) cont y
            return $ Yield z s
          Skip s -> return $ Skip s
          Done -> return Done

    {-# INLINE iterr #-}
    iterr = \(Ap fs as) cont ->
        iterr fs $ \case
          Yield f s -> \y -> do
            z <- iterr (Map (return . fmap f) as) cont y
            return $ Yield z s
          Skip s -> \_ -> return $ Skip s
          Done -> \_ -> return Done

instance Iter i => Iter ('B i) where
    {-# INLINE iterl #-}
    iterl = \(Bind as f) cont ->
        iterl as $ \y -> \case
          Yield a s ->
            case f a of
              Loop l -> do
                z <- iterl l cont y
                return $ Yield z s
          Skip s -> return $ Skip s
          Done -> return Done

    {-# INLINE iterl' #-}
    iterl' = \(Bind as f) cont ->
        iterl' as $ \ !y -> \case
          Yield a s ->
            case f a of
              Loop l -> do
                !z <- iterl l cont y
                return $ Yield z s
          Skip s -> return $ Skip s
          Done -> return Done

    {-# INLINE iterr #-}
    iterr = \(Bind as f) cont ->
        iterr as $ \case
          Yield a s ->
            case f a of
              Loop l -> \y -> do
                z <- iterr l cont y
                return $ Yield z s
          Skip s -> \_ -> return $ Skip s
          Done -> \_ -> return Done

instance (Iter i, Iter j) => Iter ('S i j) where
    {-# INLINE iterl #-}
    iterl = \(Alt i j) cont z -> iterl i cont z >>= iterl j cont

    {-# INLINE iterl' #-}
    iterl' = \(Alt i j) cont z -> iterl' i cont z >>= iterl' j cont

    {-# INLINE iterr #-}
    iterr = \(Alt i j) cont z -> iterr i cont =<< iterr j cont z

instance Iter 'Z where
    {-# INLINE iterl #-}
    iterl = \Zero cont y -> do
        r <- cont y Done
        case r of
          Yield z _ -> return z
          Skip _ -> return y
          Done -> return y

    {-# INLINE iterl' #-}
    iterl' = \Zero cont y -> do
        r <- cont y Done
        case r of
          Yield z _ -> return z
          Skip _ -> return y
          Done -> return y

    {-# INLINE iterr #-}
    iterr = \Zero cont y -> do
        r <- cont Done y
        case r of
          Yield z _ -> return z
          Skip _ -> return y
          Done -> return y

nfoldlM :: (Iter n, Monad m) => (a -> b -> m a) -> a -> LoopI n m b -> m a
{-# INLINE nfoldlM #-}
nfoldlM = \f x l ->
  iterl l (\y -> \case
    Yield a s -> f y a >>= \z -> return $ Yield z s
    Skip s -> return $ Skip s
    Done -> return Done) x

nfoldlM' :: (Iter n, Monad m) => (a -> b -> m a) -> a -> LoopI n m b -> m a
{-# INLINE nfoldlM' #-}
nfoldlM' = \f x l ->
  iterl' l (\y -> \case
    Yield a s -> f y a >>= \z -> return $ Yield z s
    Skip s -> return $ Skip s
    Done -> return Done) x

nfoldrM :: (Iter n, Monad m) => (a -> b -> m b) -> b -> LoopI n m a -> m b
{-# INLINE nfoldrM #-}
nfoldrM = \f x l ->
  iterr l (\case
    Yield a s -> \y -> f a y >>= \z -> return $ Yield z s
    Skip s -> \_ -> return $ Skip s
    Done -> \_ -> return Done) x

data Loop m a = forall (n :: LS). Iter n => Loop (LoopI n m a)

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

filterM :: Monad m => (a -> m Bool) -> Loop m a -> Loop m a
{-# INLINE filterM #-}
filterM = \check (Loop l) ->
    Loop (Map (\case
      Yield a s -> do
        p <- check a
        return $ if p then Yield a s else Skip s
      Skip s -> return $ Skip s
      Done -> return Done) l)

filter :: (a -> Bool) -> Loop m a -> Loop m a
{-# INLINE filter #-}
filter = \check -> filterM (return . check)
