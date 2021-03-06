{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Loop.ForEach (ForEach(..)) where

import Control.Monad (liftM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (lift)

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

import Control.Monad.Loop.Internal

-- | Class of containers that can be iterated over. The class is
-- parameterized over a base monad where the values of the container can be
-- read to allow iterating over mutable structures. The associated type
-- families parameterize the value and index types of the container,
-- allowing the class to be instantiated for container types (unboxed or
-- storable vectors, for example) which do not admit all types as values.
class ForEach m c where
    type ForEachValue c
    type ForEachIx c
    -- | Iterate over the values in the container.
    forEach :: Unrolling n => Unroll n -> c -> m (ForEachValue c)
    -- | Iterate over the indices and the value at each index.
    iforEach :: Unrolling n => Unroll n -> c -> m (ForEachIx c, ForEachValue c)

instance (Monad m) => ForEach (LoopLike r m) [a] where
    type ForEachValue [a] = a
    type ForEachIx [a] = Int

    forEach unr = \as -> liftM head $ for unr as (not . null) tail
    {-# INLINE forEach #-}

    iforEach unr = forEach unr . zip [0..]
    {-# INLINE iforEach #-}

instance (Monad m) => ForEach (LoopLike r m) (V.Vector a) where
    type ForEachValue (V.Vector a) = a
    type ForEachIx (V.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (Monad m, U.Unbox a) => ForEach (LoopLike r m) (U.Vector a) where
    type ForEachValue (U.Vector a) = a
    type ForEachIx (U.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (Monad m, P.Prim a) => ForEach (LoopLike r m) (P.Vector a) where
    type ForEachValue (P.Vector a) = a
    type ForEachIx (P.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (Monad m, S.Storable a) => ForEach (LoopLike r m) (S.Vector a) where
    type ForEachValue (S.Vector a) = a
    type ForEachIx (S.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

forEachVector :: (Monad m, G.Vector v a, Unrolling n) => Unroll n -> v a -> LoopLike r m a
{-# INLINE forEachVector #-}
forEachVector unr = liftM snd . iforEachVector unr

iforEachVector :: (Monad m, G.Vector v a, Unrolling n) => Unroll n -> v a -> LoopLike r m (Int, a)
{-# INLINE iforEachVector #-}
iforEachVector unr = \v -> do
    let len = G.length v
    i <- for unr 0 (< len) (+ 1)
    x <- G.unsafeIndexM v i
    return (i, x)

instance (PrimMonad m, PrimState m ~ s) => ForEach (LoopLike r m) (MV.MVector s a) where
    type ForEachValue (MV.MVector s a) = a
    type ForEachIx (MV.MVector s a) = Int
    forEach = forEachMVector
    iforEach = iforEachMVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (PrimMonad m, U.Unbox a, PrimState m ~ s) => ForEach (LoopLike r m) (MU.MVector s a) where
    type ForEachValue (MU.MVector s a) = a
    type ForEachIx (MU.MVector s a) = Int
    forEach = forEachMVector
    iforEach = iforEachMVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (PrimMonad m, P.Prim a, PrimState m ~ s) => ForEach (LoopLike r m) (MP.MVector s a) where
    type ForEachValue (MP.MVector s a) = a
    type ForEachIx (MP.MVector s a) = Int
    forEach = forEachMVector
    iforEach = iforEachMVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (S.Storable a, PrimMonad m, PrimState m ~ s) => ForEach (LoopLike r m) (MS.MVector s a) where
    type ForEachValue (MS.MVector s a) = a
    type ForEachIx (MS.MVector s a) = Int
    forEach = forEachMVector
    iforEach = iforEachMVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

forEachMVector :: (PrimMonad m, MG.MVector v a, Unrolling n) => Unroll n -> v (PrimState m) a -> LoopLike r m a
{-# INLINE forEachMVector #-}
forEachMVector unr = liftM snd . iforEachMVector unr

iforEachMVector :: (PrimMonad m, MG.MVector v a, Unrolling n) => Unroll n -> v (PrimState m) a -> LoopLike r m (Int, a)
{-# INLINE iforEachMVector #-}
iforEachMVector unr = \v -> do
    let len = MG.length v
    i <- for unr 0 (< len) (+ 1)
    x <- lift $ MG.unsafeRead v i
    return (i, x)

