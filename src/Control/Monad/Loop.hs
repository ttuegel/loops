{-# LANGUAGE CPP #-}

module Control.Monad.Loop
    ( LoopLike(..), buildLoopLike
    , LoopT(..), Loop, buildLoopT, loopT, loop, unloop, runLoopT
    , cons, continue, continue_, breaking, breaking_, unbreakable, exec_
#if __GLASGOW_HASKELL >= 708
    , ForEach(ForEachValue, ForEachIx)
#else
    , ForEach(), ForEachValue, ForEachIx
#endif
    , iterate, forever, for, unfoldl, while
    , forEach, iforEach
    ) where

import Control.Monad.Loop.Unroll
    ( LoopLike(..), buildLoopLike
    , LoopT(..), Loop, buildLoopT, loopT, loop, unloop, runLoopT
    , cons, continue, continue_, breaking, breaking_, unbreakable, exec_
#if __GLASGOW_HASKELL >= 708
    , ForEach(ForEachValue, ForEachIx)
#else
    , ForEach(), ForEachValue, ForEachIx
#endif
    , unroll1
    )
import qualified Control.Monad.Loop.Unroll as U
import Prelude hiding (break, iterate)

iterate
    :: a          -- ^ Starting value of iterator
    -> (a -> a)   -- ^ Advance the iterator
    -> LoopLike r m a
{-# INLINE iterate #-}
iterate = U.iterate unroll1

forever :: LoopLike r m ()
{-# INLINE forever #-}
forever = U.forever unroll1

for
    :: a            -- ^ Starting value of iterator
    -> (a -> Bool)  -- ^ Termination condition. The loop will terminate the
                    -- first time this is false. The termination condition
                    -- is checked at the /start/ of each iteration.
    -> (a -> a)     -- ^ Advance the iterator
    -> LoopLike r m a
{-# INLINE for #-}
for = U.for unroll1

unfoldl
    :: (i -> Maybe (i, a))  -- ^ @Just (i, a)@ advances the loop, yielding an
                            -- @a@. @Nothing@ terminates the loop.
    -> i                    -- ^ Starting value
    -> LoopLike r m a
{-# INLINE unfoldl #-}
unfoldl = U.unfoldl unroll1

while
    :: Monad m
    => m Bool
    -> LoopLike r m ()
{-# INLINE while #-}
while = U.while unroll1

-- | Iterate over the values in the container.
forEach :: ForEach m c => c -> m (ForEachValue c)
{-# INLINE forEach #-}
forEach = U.forEach unroll1

-- | Iterate over the indices and the value at each index.
iforEach :: ForEach m c => c -> m (ForEachIx c, ForEachValue c)
{-# INLINE iforEach #-}
iforEach = U.iforEach unroll1
