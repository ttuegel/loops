{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Loop.ForEach (ForEach(..)) where

import Control.Monad (liftM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (lift)
import GHC.TypeLits

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
import Control.Monad.Loop.Static
import Control.Monad.Loop.Unroll

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
    forEach :: c -> m (ForEachValue c)
    -- | Iterate over the indices and the value at each index.
    iforEach :: c -> m (ForEachIx c, ForEachValue c)
    -- | Iterate over the values in the container.
    forEachU :: (KnownNat n, Unrolling n) => Static n -> c -> m (ForEachValue c)
    -- | Iterate over the indices and the value at each index.
    iforEachU :: (KnownNat n, Unrolling n) => Static n -> c -> m (ForEachIx c, ForEachValue c)

instance (Monad m) => ForEach (LoopR r m) [a] where
    type ForEachValue [a] = a
    type ForEachIx [a] = Int

    {-# INLINE forEach #-}
    forEach = \as -> liftM head $ for as (not . null) tail

    {-# INLINE iforEach #-}
    iforEach = forEach . zip [0..]

    {-# INLINE forEachU #-}
    forEachU unr = \as -> do
        let n = fromEnum $ natVal unr
            adv (_, bs) = splitAt n bs
        (bs, rest) <- for (splitAt n as) (not . null . fst) adv
        if null rest
          then forEach bs
          else liftM head $ iterateS unr bs tail

    {-# INLINE iforEachU #-}
    iforEachU unr = forEachU unr . zip [0..]

instance (Monad m) => ForEach (LoopR r m) (V.Vector a) where
    type ForEachValue (V.Vector a) = a
    type ForEachIx (V.Vector a) = Int
    {-# INLINE forEach #-}
    forEach = forEachVector
    {-# INLINE iforEach #-}
    iforEach = iforEachVector
    {-# INLINE forEachU #-}
    forEachU = forEachUVector
    {-# INLINE iforEachU #-}
    iforEachU = iforEachUVector

instance (Monad m, U.Unbox a) => ForEach (LoopR r m) (U.Vector a) where
    type ForEachValue (U.Vector a) = a
    type ForEachIx (U.Vector a) = Int
    {-# INLINE forEach #-}
    forEach = forEachVector
    {-# INLINE iforEach #-}
    iforEach = iforEachVector
    {-# INLINE forEachU #-}
    forEachU = forEachUVector
    {-# INLINE iforEachU #-}
    iforEachU = iforEachUVector

instance (Monad m, P.Prim a) => ForEach (LoopR r m) (P.Vector a) where
    type ForEachValue (P.Vector a) = a
    type ForEachIx (P.Vector a) = Int
    {-# INLINE forEach #-}
    forEach = forEachVector
    {-# INLINE iforEach #-}
    iforEach = iforEachVector
    {-# INLINE forEachU #-}
    forEachU = forEachUVector
    {-# INLINE iforEachU #-}
    iforEachU = iforEachUVector

instance (Monad m, S.Storable a) => ForEach (LoopR r m) (S.Vector a) where
    type ForEachValue (S.Vector a) = a
    type ForEachIx (S.Vector a) = Int
    {-# INLINE forEach #-}
    forEach = forEachVector
    {-# INLINE iforEach #-}
    iforEach = iforEachVector
    {-# INLINE forEachU #-}
    forEachU = forEachUVector
    {-# INLINE iforEachU #-}
    iforEachU = iforEachUVector

forEachVector :: (Monad m, G.Vector v a) => v a -> LoopR r m a
{-# INLINE forEachVector #-}
forEachVector = \v -> numFromN 0 (G.length v) >>= G.basicUnsafeIndexM v

iforEachVector :: (Monad m, G.Vector v a) => v a -> LoopR r m (Int, a)
{-# INLINE iforEachVector #-}
iforEachVector = \v -> do
    let len = G.length v
    i <- numFromN 0 len
    x <- G.unsafeIndexM v i
    return (i, x)

forEachUVector :: (G.Vector v a, KnownNat n, Monad m, Unrolling n) => Static n -> v a -> LoopR r m a
{-# INLINE forEachUVector #-}
forEachUVector unr = liftM snd . iforEachUVector unr

iforEachUVector :: (G.Vector v a, KnownNat n, Monad m, Unrolling n) => Static n -> v a -> LoopR r m (Int, a)
{-# INLINE iforEachUVector #-}
iforEachUVector unr = \v -> do
    let len = G.length v
    i <- numFromNU unr 0 len
    x <- G.unsafeIndexM v i
    return (i, x)

instance (PrimMonad m, PrimState m ~ s) => ForEach (LoopR r m) (MV.MVector s a) where
    type ForEachValue (MV.MVector s a) = a
    type ForEachIx (MV.MVector s a) = Int
    {-# INLINE forEach #-}
    forEach = forEachMVector
    {-# INLINE iforEach #-}
    iforEach = iforEachMVector
    {-# INLINE forEachU #-}
    forEachU = forEachUMVector
    {-# INLINE iforEachU #-}
    iforEachU = iforEachUMVector

instance (PrimMonad m, U.Unbox a, PrimState m ~ s) => ForEach (LoopR r m) (MU.MVector s a) where
    type ForEachValue (MU.MVector s a) = a
    type ForEachIx (MU.MVector s a) = Int
    {-# INLINE forEach #-}
    forEach = forEachMVector
    {-# INLINE iforEach #-}
    iforEach = iforEachMVector
    {-# INLINE forEachU #-}
    forEachU = forEachUMVector
    {-# INLINE iforEachU #-}
    iforEachU = iforEachUMVector

instance (PrimMonad m, P.Prim a, PrimState m ~ s) => ForEach (LoopR r m) (MP.MVector s a) where
    type ForEachValue (MP.MVector s a) = a
    type ForEachIx (MP.MVector s a) = Int
    {-# INLINE forEach #-}
    forEach = forEachMVector
    {-# INLINE iforEach #-}
    iforEach = iforEachMVector
    {-# INLINE forEachU #-}
    forEachU = forEachUMVector
    {-# INLINE iforEachU #-}
    iforEachU = iforEachUMVector

instance (S.Storable a, PrimMonad m, PrimState m ~ s) => ForEach (LoopR r m) (MS.MVector s a) where
    type ForEachValue (MS.MVector s a) = a
    type ForEachIx (MS.MVector s a) = Int
    {-# INLINE forEach #-}
    forEach = forEachMVector
    {-# INLINE iforEach #-}
    iforEach = iforEachMVector
    {-# INLINE forEachU #-}
    forEachU = forEachUMVector
    {-# INLINE iforEachU #-}
    iforEachU = iforEachUMVector

forEachMVector :: (PrimMonad m, MG.MVector v a) => v (PrimState m) a -> LoopR r m a
{-# INLINE forEachMVector #-}
forEachMVector = liftM snd . iforEachMVector

iforEachMVector :: (PrimMonad m, MG.MVector v a) => v (PrimState m) a -> LoopR r m (Int, a)
{-# INLINE iforEachMVector #-}
iforEachMVector = \v -> do
    let len = MG.length v
    i <- numFromN 0 len
    x <- lift $ MG.unsafeRead v i
    return (i, x)

forEachUMVector :: (MG.MVector v a, KnownNat n, PrimMonad m, Unrolling n) => Static n -> v (PrimState m) a -> LoopR r m a
{-# INLINE forEachUMVector #-}
forEachUMVector unr = liftM snd . iforEachUMVector unr

iforEachUMVector :: (MG.MVector v a, KnownNat n, PrimMonad m, Unrolling n) => Static n -> v (PrimState m) a -> LoopR r m (Int, a)
{-# INLINE iforEachUMVector #-}
iforEachUMVector unr = \v -> do
    let len = MG.length v
    i <- numFromNU unr 0 len
    x <- lift $ MG.unsafeRead v i
    return (i, x)
