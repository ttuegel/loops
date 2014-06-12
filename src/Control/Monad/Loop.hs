module Control.Monad.Loop
    ( LoopT(..), Loop, loop
    , cons, continue, continue_, break_, exec_
    , ForEach(ForEachValue, ForEachIx)
    , iterate, forever, for, unfoldl, while
    , forEach, iforEach
    ) where

import Control.Monad.Loop.Unroll
    ( LoopT(..), Loop, loop
    , cons, continue, continue_, break_, exec_
    , ForEach(ForEachValue, ForEachIx)
    , noUnroll
    )
import qualified Control.Monad.Loop.Unroll as U
import Prelude hiding (iterate)

iterate
    :: a          -- ^ Starting value of iterator
    -> (a -> a)   -- ^ Advance the iterator
    -> LoopT m a
{-# INLINE iterate #-}
iterate = U.iterate noUnroll

forever :: LoopT m ()
{-# INLINE forever #-}
forever = U.forever noUnroll

for
    :: a            -- ^ Starting value of iterator
    -> (a -> Bool)  -- ^ Termination condition. The loop will terminate the
                    -- first time this is false. The termination condition
                    -- is checked at the /start/ of each iteration.
    -> (a -> a)     -- ^ Advance the iterator
    -> LoopT m a
{-# INLINE for #-}
for = U.for noUnroll

unfoldl
    :: (i -> Maybe (i, a))  -- ^ @Just (i, a)@ advances the loop, yielding an
                            -- @a@. @Nothing@ terminates the loop.
    -> i                    -- ^ Starting value
    -> LoopT m a
{-# INLINE unfoldl #-}
unfoldl = U.unfoldl noUnroll

while
    :: Monad m
    => m Bool
    -> LoopT m ()
{-# INLINE while #-}
while = U.while noUnroll

-- | Iterate over the values in the container.
forEach :: ForEach m c => c -> m (ForEachValue c)
{-# INLINE forEach #-}
forEach = U.forEach noUnroll

-- | Iterate over the indices and the value at each index.
iforEach :: ForEach m c => c -> m (ForEachIx c, ForEachValue c)
{-# INLINE iforEach #-}
iforEach = U.iforEach noUnroll
