import Criterion.Main
import Data.Foldable (foldl')
import Data.Functor.Identity
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Control.Monad.Loop (Loop)
import qualified Control.Monad.Loop as L

main :: IO ()
main = do
  defaultMain [ bench "Vector/unfused" $ whnf mapFilterSumV (V.generate n id)
              , bench "Loop/fused" $ whnf mapFilterSumL n
              ]
  where
    n = 1000000

mapFilterSumV :: Vector Int -> Int
{-# INLINE mapFilterSumV #-}
mapFilterSumV = V.sum . V.filter (< 10) . V.map (+ 5)

mapFilterSumL :: Int -> Int
{-# INLINE mapFilterSumL #-}
mapFilterSumL n = foldl' (+) 0 $ L.filter (< 10) $ fmap (+ 5) loop
  where
    loop :: Loop Identity Int
    loop = L.generate n id
