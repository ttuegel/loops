{-# LANGUAGE Rank2Types #-}

module Control.Monad.Loop where

import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(mempty, mappend))
import Prelude hiding (foldr)

newtype LoopT m a =
  LoopT { runLoopT :: forall r. (a -> m r -> m r) -> m r -> m r }

instance Functor (LoopT m) where
  {-# INLINE fmap #-}
  fmap = \f l -> LoopT $ \yield -> runLoopT l (yield . f)

instance Applicative (LoopT m) where
  {-# INLINE pure #-}
  pure = \x -> LoopT $ \yield -> yield x

  {-# INLINE (<*>) #-}
  (<*>) = \fs as -> LoopT $ \yield ->
    runLoopT fs $ \f -> runLoopT as (yield . f)

instance Monad (LoopT m) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  (>>=) = \xs f -> LoopT $ \yield -> runLoopT xs $ \x -> runLoopT (f x) yield

instance Monoid (LoopT m a) where
  {-# INLINE mempty #-}
  mempty = LoopT $ \_ next -> next

  {-# INLINE mappend #-}
  mappend = \xs ys -> LoopT $ \yield next ->
    runLoopT xs yield $ runLoopT ys yield next

instance MonadTrans LoopT where
  {-# INLINE lift #-}
  lift = \inner -> LoopT $ \yield next -> inner >>= \a -> yield a next

instance MonadIO m => MonadIO (LoopT m) where
  {-# INLINE liftIO #-}
  liftIO = lift . liftIO

instance (Applicative m, Foldable m) => Foldable (LoopT m) where
  {-# INLINE foldr #-}
  foldr f z l = foldr (.) id inner z where
    inner = runLoopT l (\a next -> (f a .) <$> next) $ pure id
