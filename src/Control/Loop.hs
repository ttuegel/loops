{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Loop
    ( Loop
    , unfoldl, for
    , module Control.Monad.Trans.Class
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Free.Church
import Data.Bifunctor
import Data.Foldable
import Data.Strict.Maybe
import Prelude hiding (Maybe(..))

data LoopOp r = forall i. Unfold i (i -> Maybe (i, r))

instance Functor LoopOp where
    fmap f op =
        case op of
          Unfold i0 unf -> Unfold i0 $ fmap (second f) . unf

type Loop = F LoopOp

unfoldl :: (i -> Maybe (i, r)) -> i -> Loop r
unfoldl unf i0 = liftF $ Unfold i0 unf

for :: i -> (i -> Bool) -> (i -> i) -> Loop i
for i0 cont next = unfoldl unf i0
  where
    unf i | cont i = Just (next i, i)
          | otherwise = Nothing

instance Foldable (F LoopOp) where
    foldr f r0 loop = runF loop f go r0
      where
        go (Unfold i0 unf) =
            let going i =
                    case unf i of
                      Nothing -> id
                      Just (i', r) -> r . going i'
            in going i0
