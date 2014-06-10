{-# LANGUAGE ExistentialQuantification #-}

module Control.Loop.LoopPrim where

import Data.Foldable (Foldable(..))

-- | Primitive commands in the language of loops
data LoopPrim a = forall b. For b (b -> Bool) (b -> b) (b -> a) | Continue

instance Functor LoopPrim where
    fmap f prim =
      case prim of
        For i0 check next g -> For i0 check next (f . g)
        Continue -> Continue
    {-# INLINE fmap #-}

instance Foldable LoopPrim where
    foldr f r0 prim =
      case prim of
        For i0 check next g ->
          let _for i | check i = f (g i) $ _for $ next i
                     | otherwise = r0
          in _for i0
        Continue -> r0
    {-# INLINE foldr #-}

    foldl' f r0 prim =
      case prim of
        For i0 check next g ->
          let _for i r
                | check i =
                  let i' = next i
                      r' = f r $! g i
                  in i' `seq` r' `seq` _for i' r'
                | otherwise = r
          in _for i0 r0
        Continue -> r0
    {-# INLINE foldl' #-}
