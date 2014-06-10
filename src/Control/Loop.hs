module Control.Loop
    (
    -- * Loops
      Loop, LoopPrim(..)
    , continue, for, unfoldl
    -- * Iterating over containers
    , ForEach(..)
    ) where

import Control.Loop.Prim hiding (continue, for, unfoldl)
import qualified Control.Loop.Prim as Prim
import Control.Loop.ForEach

for :: i -> (i -> Bool) -> (i -> i) -> Loop i
for = Prim.for
{-# INLINE for #-}

unfoldl :: (i -> Maybe (i, r)) -> i -> Loop r
unfoldl = Prim.unfoldl
{-# INLINE unfoldl #-}

-- | Emit no value in the current iteration and skip to the start of the
-- next iteration.
continue :: Loop r
continue = Prim.continue
{-# INLINE continue #-}
