module Control.Monad.Loop.Class where

class Monad m => MonadLoop m where
    iterate :: a -> (a -> a) -> m a
    --for :: a -> (a -> Bool) -> (a -> a) -> m a
