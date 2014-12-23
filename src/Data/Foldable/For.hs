{-# LANGUAGE BangPatterns #-}

module Data.Foldable.For where

import Data.Foldable

data For a = F !a !(a -> Bool) !(a -> a)

instance Foldable For where
    foldr = \f r (F a0 check next) ->
        let foldr_For_go !a
              | check a = f a $ foldr_For_go $! next a
              | otherwise = r
        in foldr_For_go a0
    {-# INLINE foldr #-}

    foldl' = \f r0 (F a0 check next) ->
        let foldl'_For_go !r !a
              | check a =
                  let !r' = f r a
                      !a' = next a
                  in foldl'_For_go r' a'
              | otherwise = r
        in foldl'_For_go r0 a0
    {-# INLINE foldl' #-}
