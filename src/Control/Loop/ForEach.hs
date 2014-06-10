{-# LANGUAGE TypeFamilies #-}

module Control.Loop.ForEach where

import Control.Applicative ((<$>))

-- Import the vector package qualified to write the ForEach instances
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

import Control.Loop.Internal

-- | Class of containers that can be iterated over. The class is
-- parameterized over a base monad where the values of the container can be
-- read to allow iterating over mutable structures. The associated type
-- families parameterize the value and index types of the container,
-- allowing the class to be instantiated for container types (unboxed or
-- storable vectors, for example) which do not admit all types as values.
class ForEach c where
    type ForEachValue c
    type ForEachIx c
    -- | Iterate over the values in the container.
    forEach :: c -> Loop (ForEachValue c)
    -- | Iterate over the indices and the value at each index.
    iforEach :: c -> Loop (ForEachIx c, ForEachValue c)

instance ForEach [a] where
    type ForEachValue [a] = a
    type ForEachIx [a] = Int

    forEach as = head <$> for as (not . null) tail
    {-# INLINE forEach #-}

    iforEach = forEach . zip [0..]
    {-# INLINE iforEach #-}

instance ForEach (V.Vector a) where
    type ForEachValue (V.Vector a) = a
    type ForEachIx (V.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (U.Unbox a) => ForEach (U.Vector a) where
    type ForEachValue (U.Vector a) = a
    type ForEachIx (U.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (P.Prim a) => ForEach (P.Vector a) where
    type ForEachValue (P.Vector a) = a
    type ForEachIx (P.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

instance (S.Storable a) => ForEach (S.Vector a) where
    type ForEachValue (S.Vector a) = a
    type ForEachIx (S.Vector a) = Int
    forEach = forEachVector
    iforEach = iforEachVector
    {-# INLINE forEach #-}
    {-# INLINE iforEach #-}

forEachVector :: (G.Vector v a) => v a -> Loop a
{-# INLINE forEachVector #-}
forEachVector = fmap snd . iforEachVector

iforEachVector :: (G.Vector v a) => v a -> Loop (Int, a)
{-# INLINE iforEachVector #-}
iforEachVector v = do
    let len = G.length v
    i <- for 0 (< len) (+ 1)
    x <- G.unsafeIndexM v i
    return (i, x)
