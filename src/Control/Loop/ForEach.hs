{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Loop.ForEach where

import Control.Applicative ((<$>))
import Control.Monad.Free.Church
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free.Church hiding (F, fromF, runF)
import Control.Monad.Primitive (PrimMonad, PrimState)

-- Import the vector package qualified to write the ForEach instances
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as MP
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Control.Loop.Prim

-- | Class of containers that can be iterated over. The type of indices and
-- values in the container are parameterized by associated type families to
-- allow iterating over containers which place restrictions on the types of
-- their values (like the types from the @vector@ package).
class MonadFree LoopPrim m => ForEach m c where
    type ForEachValue c
    type ForEachIx c
    -- | Iterate over the values in the container.
    forEach :: c -> m (ForEachValue c)
    -- | Iterate over the indices and the value at each index.
    iforEach :: c -> m (ForEachIx c, ForEachValue c)

instance (Functor m, MonadFree LoopPrim m) => ForEach m [a] where
    type ForEachValue [a] = a
    type ForEachIx [a] = Int

    forEach as = head <$> for as (not . null) tail
    {-# INLINE forEach #-}

    iforEach = forEach . zip [0..]
    {-# INLINE iforEach #-}

instance (Functor m, MonadFree LoopPrim m, G.Vector V.Vector a) => ForEach m (V.Vector a) where
    type ForEachValue (V.Vector a) = a
    type ForEachIx (V.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (Functor m, MonadFree LoopPrim m, G.Vector U.Vector a) => ForEach m (U.Vector a) where
    type ForEachValue (U.Vector a) = a
    type ForEachIx (U.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (Functor m, MonadFree LoopPrim m, G.Vector P.Vector a) => ForEach m (P.Vector a) where
    type ForEachValue (P.Vector a) = a
    type ForEachIx (P.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (Functor m, MonadFree LoopPrim m, G.Vector P.Vector a, S.Storable a) => ForEach m (S.Vector a) where
    type ForEachValue (S.Vector a) = a
    type ForEachIx (S.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

forEachVector :: (Functor m, MonadFree LoopPrim m, G.Vector v a) => v a -> m a
{-# INLINE forEachVector #-}
forEachVector = fmap snd . iforEachVector

iforEachVector :: (MonadFree LoopPrim m, G.Vector v a) => v a -> m (Int, a)
{-# INLINE iforEachVector #-}
iforEachVector v = do
    let len = G.length v
    i <- for 0 (< len) (+ 1)
    x <- G.unsafeIndexM v i
    return (i, x)

instance (Functor m, PrimMonad m, MG.MVector MV.MVector a, PrimState m ~ s) => ForEach (FT LoopPrim m) (MV.MVector s a) where
    type ForEachValue (MV.MVector s a) = a
    type ForEachIx (MV.MVector s a) = Int
    forEach = forEachMVector
    iforEach = iforEachMVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (Functor m, PrimMonad m, MG.MVector MU.MVector a, PrimState m ~ s) => ForEach (FT LoopPrim m) (MU.MVector s a) where
    type ForEachValue (MU.MVector s a) = a
    type ForEachIx (MU.MVector s a) = Int
    forEach = forEachMVector
    iforEach = iforEachMVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (Functor m, PrimMonad m, MG.MVector MP.MVector a, PrimState m ~ s) => ForEach (FT LoopPrim m) (MP.MVector s a) where
    type ForEachValue (MP.MVector s a) = a
    type ForEachIx (MP.MVector s a) = Int
    forEach = forEachMVector
    iforEach = iforEachMVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (S.Storable a, Functor m, PrimMonad m, MG.MVector MS.MVector a, PrimState m ~ s) => ForEach (FT LoopPrim m) (MS.MVector s a) where
    type ForEachValue (MS.MVector s a) = a
    type ForEachIx (MS.MVector s a) = Int
    forEach = forEachMVector
    iforEach = iforEachMVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

forEachMVector :: (Functor m, PrimMonad m, MG.MVector v a) => v (PrimState m) a -> LoopT m a
{-# INLINE forEachMVector #-}
forEachMVector = fmap snd . iforEachMVector

iforEachMVector :: (PrimMonad m, MG.MVector v a) => v (PrimState m) a -> LoopT m (Int, a)
{-# INLINE iforEachMVector #-}
iforEachMVector v = do
    let len = MG.length v
    i <- for 0 (< len) (+ 1)
    x <- lift $ MG.unsafeRead v i
    return (i, x)
