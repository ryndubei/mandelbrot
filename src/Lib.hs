module Lib (inMandelbrotSet) where
import Data.Complex
import Data.List (genericTake)

-- Estimate whether a point is in the Mandelbrot set, using at most
-- the given number of iterations.
inMandelbrotSet :: RealFloat a => Integer -> Complex a -> Bool
inMandelbrotSet iterations point =
  let iteration = genericTake (iterations + 1) $ iterate (\z -> z*z + point) 0
   in all ((< 2) . magnitude) iteration
