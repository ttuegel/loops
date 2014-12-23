{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Foldable.For where

import Data.Foldable

data For a = forall x. F !x !(x -> Bool) !(x -> (# x, a #))

instance Functor For where
    fmap = \f (F x check next) ->
        let next' y = let (# !z, a #) = next y in (# z, f a #)
        in F x check next'
    {-# INLINE fmap #-}

instance Foldable For where
    foldr = \f r (F x0 check next) ->
        let foldr_For_go !x
              | check x =
                  let (# !y, a #) = next x
                  in f a $ foldr_For_go y
              | otherwise = r
        in foldr_For_go x0
    {-# INLINE foldr #-}

    foldl' = \f r0 (F x0 check next) ->
        let foldl'_For_go !r !x
              | check x =
                  let (# !y, !a #) = next x
                      !s = f r a
                  in foldl'_For_go s y
              | otherwise = r
        in foldl'_For_go r0 x0
    {-# INLINE foldl' #-}

for :: a -> (a -> Bool) -> (a -> a) -> For a
for = \a0 check next -> F a0 check $ \a -> (# next a, a #)
{-# INLINE for #-}

skip :: Int -> For a -> For a
skip = \n0 (F x0 check next) ->
    let skip_go !n !x
          | n > 0 && check x =
              let (# y, _ #) = next x
              in skip_go (n - 1) y
          | otherwise = F x check next
    in skip_go n0 x0
{-# INLINE skip #-}
