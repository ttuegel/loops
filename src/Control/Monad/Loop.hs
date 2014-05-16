{-# LANGUAGE ExistentialQuantification #-}

module Control.Monad.Loop
    ( LoopT, loopT
    , unfoldl, for
    , module Control.Monad.Trans.Class
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Free (liftF)
import Control.Monad.Trans.Free.Church
import Data.Bifunctor
import Data.Strict.Maybe
import Data.Strict.Tuple
import Prelude hiding (Maybe(..))

instance Bifunctor Pair where
    bimap f g (a :!: b) = f a :!: g b

data LoopOp r = forall i. Unfold i (i -> Maybe (Pair i r))

instance Functor LoopOp where
    fmap f op =
        case op of
          Unfold i0 unf -> Unfold i0 $ fmap (second f) . unf

type LoopT = FT LoopOp

loopT :: Monad m => LoopT m () -> m ()
loopT = iterT go
  where
    go (Unfold i0 unf) =
        let unfolding i =
                case unf i of
                  Nothing -> return ()
                  Just (i' :!: act) -> act >> unfolding i'
        in unfolding i0

unfoldl :: Monad m => (i -> Maybe (Pair i r)) -> i -> LoopT m r
unfoldl unf i0 = liftF $ Unfold i0 unf

for :: (Monad m, Show i) => i -> (i -> Bool) -> (i -> i) -> LoopT m i
for i0 cont next = unfoldl unf i0
  where
    unf i | cont i = Just $! next i :!: i
          | otherwise = Nothing
