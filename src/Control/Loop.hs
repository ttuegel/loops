{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Loop
    ( Loop, LoopT, LoopPrim(..)
    , for, unfoldl
    , module Control.Monad.Trans.Class
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Free (Free(..))
import Control.Monad.Free.Church
import Control.Monad.Trans.Free.Church hiding (F, fromF)
import Data.Foldable
import Data.Maybe (fromJust, isJust)
import Prelude hiding (foldr)

data LoopPrim a = For a (a -> Bool) (a -> a) | forall b. Map (LoopPrim b) (b -> a)

instance Functor LoopPrim where
    fmap f prim =
      case prim of
        Map prim' g -> Map prim' (f . g)
        _ -> Map prim f
    {-# INLINE fmap #-}

instance Foldable LoopPrim where
    foldr f r0 prim =
      case prim of
        Map prim' g -> foldr (f . g) r0 prim'
        For i0 check next ->
          let go i | check i = f i $ go $ next i
                   | otherwise = r0
          in go i0
    {-# INLINE foldr #-}

    foldl' = go where
      go :: (r -> a -> r) -> r -> LoopPrim a -> r
      go f r0 prim =
        case prim of
          Map prim' g -> go (\r -> f r . g) r0 prim'
          For i0 check next ->
            let _for i r
                  | check i =
                    let i' = next i
                        r' = f r i
                    in i' `seq` r' `seq` _for i' r'
                  | otherwise = r
            in _for i0 r0
    {-# INLINE foldl' #-}

type Loop = F LoopPrim
type LoopT = FT LoopPrim

for :: MonadFree LoopPrim m => i -> (i -> Bool) -> (i -> i) -> m i
for i0 check next = liftF $ For i0 check next
{-# INLINE for #-}

unfoldl :: (Functor m, MonadFree LoopPrim m) => (i -> Maybe (i, r)) -> i -> m r
unfoldl unf i0 = fmap (fromJust . fmap snd) $ for (unf i0) isJust (>>= unf . fst)
{-# INLINE unfoldl #-}

instance (Foldable f, Functor f) => Foldable (F f) where
    foldr f r = foldr f r . (fromF :: Functor f => F f a -> Free f a)
    {-# INLINE foldr #-}

    foldl' f r = foldl' f r . (fromF :: Functor f => F f a -> Free f a)
    {-# INLINE foldl' #-}
