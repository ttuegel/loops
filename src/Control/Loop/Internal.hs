module Control.Loop.Internal where

import Control.Applicative ((<$>))
import Control.Monad.Free.Class (liftF)
import Control.Monad.Free.Church (F)
import Data.Maybe (fromJust, isJust)

import Control.Loop.LoopPrim

-- | Pure loop, usually executed through the 'Foldable' instance of 'F'.
type Loop = F LoopPrim

for :: i -> (i -> Bool) -> (i -> i) -> Loop i
for i0 check next = liftF $ For i0 check next id
{-# INLINE for #-}

unfoldl :: (i -> Maybe (i, r)) -> i -> Loop r
unfoldl unf i0 = fromJust . fmap snd <$> for (unf i0) isJust (>>= unf . fst)
{-# INLINE unfoldl #-}

-- | Emit no value in the current iteration and skip to the start of the
-- next iteration.
continue :: Loop r
continue = liftF Continue
{-# INLINE continue #-}
