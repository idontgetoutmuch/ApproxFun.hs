module ChebyshevPoints where

import Prelude hiding ( map )
import Data.Vector.Unboxed ( Vector, map, enumFromN
                           , enumFromStepN, Unbox, singleton
                           )
-- | Chebyshev points of the first kind aka Gauss-Chebyshev points
chebyshevpoints1 :: (Floating a, Unbox a) => Int -> Vector a
chebyshevpoints1 n = map (\k -> cos (pi * ((1.0 / 2+k) / n')))
                         (enumFromStepN (-(n')) 1 n)
  where
    n' = fromIntegral n

-- | Chebyshev points of the second kind aka Chebyshev-Lobatto points
chebyshevpoints2  :: (Floating a, Unbox a) => Int -> Vector a
chebyshevpoints2 n | n == 1    = singleton 0.0
                   | otherwise = map (\k -> cos (pi * (k / (n' - 1))))
                                 (enumFromStepN (n' - 1) (-1) n)
  where
    n' = fromIntegral n
