{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

module ChebyshevTransform where
import Prelude hiding ( reverse, (++), length, replicate
                      , zipWith, map, take
                      )
import Data.Complex
import Data.Vector.Unboxed
import Numeric.FFT

-- | Convert values of a function at Chebyshev points (of the second
-- kind) to co-efficients of the Chebyshev polynomials. Section 4.7
-- Mason & Handscomb, "Chebyshev Polynomials". Chapman & Hall/CRC
-- (2003).
chebVals :: Unbox a => Vector a -> (a -> Complex Double) -> IO (Vector Double)
chebVals ps f = do
  fcs <- ifft $ map f ps'
  return $ map realPart $ zipWith (*) ns (take l fcs)
  where
    ns = 1 `cons` (replicate (l - 2) 2) `snoc` 1
    l = length ps
    l' = fromIntegral l - 1
    ps' = ps ++ reverse (slice 1 (l - 2) ps)
