{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}


module Data.Functor.ContId where


import Control.Applicative
import Data.Foldable
import Data.Traversable


newtype ContId a = ContId { runContId_ :: forall r. (a -> r) -> r }


runContId :: ContId a -> (a -> r) -> r
runContId = \(!ca) (!f) -> runContId_ ca f


instance Functor ContId where
    {-# INLINE fmap #-}
    fmap = \f cid -> ContId $ \yield -> runContId cid (yield . f)


instance Applicative ContId where
    {-# INLINE pure #-}
    pure = \x -> ContId $ \yield -> yield x

    {-# INLINE (<*>) #-}
    (<*>) = \cf cx -> ContId $ \yield -> yield $ runContId cx $ runContId cf id


instance Monad ContId where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = runContId


instance Foldable ContId where
    {-# INLINE foldr #-}
    foldr = \f r0 ca -> runContId ca f r0

    {-# INLINE foldl' #-}
    foldl' = \(!f) (!r0) ca -> runContId ca (\(!a) -> f r0 a)


instance Traversable ContId where
    {-# INLINE traverse #-}
    traverse = \f ca -> fmap return (runContId ca f)
