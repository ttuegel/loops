{-# LANGUAGE RankNTypes #-}

module Control.Loop.Internal where

import Control.Applicative (Applicative(..), (<$>))
import Control.Category ((>>>))
import Data.Foldable (Foldable(..))
import Data.Maybe (fromJust, isJust)
import Data.Profunctor (lmap)
import Prelude hiding (iterate)

newtype Loop a = Loop { runLoop :: forall r. (a -> r -> r -> r) -> r -> r -> r }

instance Functor Loop where
    {-# INLINE fmap #-}
    fmap f loop = Loop $ \yield -> runLoop loop (lmap f yield)

instance Applicative Loop where
    {-# INLINE pure #-}
    pure a = Loop $ \yield -> yield a
    {-# INLINE (<*>) #-}
    fs <*> as = Loop $ \yield next ->
        runLoop fs (\f next' _ -> runLoop (fmap f as) yield next' next) next

instance Monad Loop where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    as >>= f = Loop $ \yield next ->
        runLoop as (\a next' _ -> runLoop (f a) yield next' next) next

instance Foldable Loop where
    {-# INLINE foldr #-}
    foldr f r xs = runLoop xs (\a b _ -> f a b) r r
    {-# INLINE foldl' #-}
    foldl' f r xs = runLoop xs (\a next _ -> flip f a !>>> next) id id r
      where (!>>>) h g = h >>> (g $!)

for :: a -> (a -> Bool) -> (a -> a) -> Loop a
{-# INLINE for #-}
for a0 cond adv = iterate a0 adv >>= \a -> if cond a then return a else break_

iterate :: a -> (a -> a) -> Loop a
{-# INLINE iterate #-}
iterate a0 adv = Loop $ \yield next _ ->
    let yield' a r = yield a r next
        go a = yield' a $ go $ adv a
    in go a0

unfoldl :: (i -> Maybe (i, a)) -> i -> Loop a
{-# INLINE unfoldl #-}
unfoldl unf i0 = fromJust . fmap snd <$> for (unf i0) isJust (>>= unf . fst)

continue :: a -> Loop a
{-# INLINE continue #-}
continue a = Loop $ \yield next -> yield a next

continue_ :: Loop a
{-# INLINE continue_ #-}
continue_ = Loop $ \_ next _ -> next

break_ :: Loop a
{-# INLINE break_ #-}
break_ = Loop $ \_ _ brk -> brk
