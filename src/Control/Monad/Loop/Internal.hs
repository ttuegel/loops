{-# LANGUAGE ExistentialQuantification #-}

module Control.Monad.Loop.Internal where

import Control.Applicative ((<$>))
import Control.Monad.Free.Class (liftF)
import Control.Monad.Trans.Free.Church (F, FT(..), iterT)
import Data.Foldable (Foldable(..))
import Data.Maybe (fromJust, isJust)

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

-- | Loop monad transformer, i.e., loops in another monad. Loops are
-- executed in the underlying monad with 'loopT'.
type LoopT = FT LoopPrim

-- | Pure loop, usually executed through the 'Foldable' instance of 'F'.
type Loop = F LoopPrim

for :: Monad m => i -> (i -> Bool) -> (i -> i) -> LoopT m i
for i0 check next = liftF $ For i0 check next id
{-# INLINE for #-}

unfoldl :: (Functor m, Monad m) => (i -> Maybe (i, r)) -> i -> LoopT m r
unfoldl unf i0 = fromJust . fmap snd <$> for (unf i0) isJust (>>= unf . fst)
{-# INLINE unfoldl #-}

-- | Emit no value in the current iteration and skip to the start of the
-- next iteration.
continue :: Monad m => LoopT m r
continue = liftF Continue
{-# INLINE continue #-}

-- | Run a loop monad transformer in the underlying monad.
loopT :: Monad m => LoopT m () -> m ()
loopT = iterT $ foldl' (>>) (return ())
{-# INLINE loopT #-}

