{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}

module Data.Functor.UnboxIdentity where


import Control.Applicative
import Data.Foldable
import Data.Traversable


newtype UnboxIdentity a = UnboxIdentity (forall r. r -> (# a #))


instance Functor UnboxIdentity where
    {-# INLINE fmap #-}
    fmap = \ !f (UnboxIdentity !go) -> let (# !a #) = go () in pure $! f a


instance Applicative UnboxIdentity where
    {-# INLINE pure #-}
    pure = \ !a -> UnboxIdentity (\_ -> (# a #))

    {-# INLINE (<*>) #-}
    (<*>) = \(UnboxIdentity !ff) !aa -> let (# !f #) = ff () in fmap f aa


instance Monad UnboxIdentity where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \(UnboxIdentity !aa) !f -> let (# !a #) = aa () in f a


instance Foldable UnboxIdentity where
    {-# INLINE foldr #-}
    foldr = \ !f !r (UnboxIdentity !aa) -> let (# !a #) = aa () in f a r

    {-# INLINE foldl' #-}
    foldl' = \ !f !r (UnboxIdentity !aa) -> let (# !a #) = aa () in f r a


instance Traversable UnboxIdentity where
    {-# INLINE traverse #-}
    traverse = \ !f (UnboxIdentity !aa) -> let (# !a #) = aa () in fmap pure $! f a
