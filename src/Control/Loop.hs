{-# LANGUAGE RankNTypes #-}

module Control.Loop where

import Control.Applicative (Applicative(..), (<$>))
import Control.Category ((>>>))
import Data.Foldable (Foldable(..))
import Data.Maybe (fromJust, isJust)
import Data.Profunctor (lmap)

newtype Loop a = Loop { runLoop :: forall r. (a -> r -> r) -> r -> r }

instance Functor Loop where
    {-# INLINE fmap #-}
    fmap f loop = Loop $ \yield next -> runLoop loop (lmap f yield) next

instance Applicative Loop where
    {-# INLINE pure #-}
    pure a = Loop $ \yield -> yield a
    {-# INLINE (<*>) #-}
    fs <*> as = Loop $ \yield -> runLoop fs $ \f -> runLoop (fmap f as) yield

instance Monad Loop where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    as >>= f = Loop $ \yield -> runLoop as $ \a -> runLoop (f a) yield

instance Foldable Loop where
    {-# INLINE foldr #-}
    foldr f r xs = runLoop xs f r
    {-# INLINE foldl' #-}
    foldl' f r xs = runLoop xs (\a -> (flip f a !>>>)) id r
      where (!>>>) h g = h >>> (g $!)

for :: a -> (a -> Bool) -> (a -> a) -> Loop a
{-# INLINE for #-}
{-
- The body of this loop was originally:
-
->  let go a | cond a = yield a $ go $ adv a
->           | otherwise = next
->  in go start
-
- but GHC needed -fspec-constr (-O2) to optimize correctly. In particular,
- the strictness of the accumulator in foldl' was not being detected; the
- loop would box and unbox the accumulator on every iteration. Rather than
- count on users to enable particular flags, I thought it made more sense
- (for a simple function) to perform the call-pattern specialization by
- hand. This induces a small overhead in empty loops.
-}
for start cond adv
    | cond start = Loop $ \yield next ->
        let go a = yield a $ let a' = adv a in if cond a' then go a' else next
        in go start
    | otherwise = continue_

unfoldl :: (i -> Maybe (i, a)) -> i -> Loop a
{-# INLINE unfoldl #-}
unfoldl unf i0 = fromJust . fmap snd <$> for (unf i0) isJust (>>= unf . fst)

continue :: a -> Loop a
{-# INLINE continue #-}
continue a = Loop $ \yield next -> yield a next

continue_ :: Loop a
{-# INLINE continue_ #-}
continue_ = Loop $ \_ next -> next
