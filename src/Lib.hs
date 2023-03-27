module Lib (inMandelbrotSet) where
import Data.Complex
import Data.List (genericTake, findIndex)
import Data.Maybe ( fromMaybe )

-- Estimate whether a point is in the Mandelbrot set, using at most
-- the given number of iterations.
inMandelbrotSet :: RealFloat a => Integer -> Complex a -> Int
inMandelbrotSet iterations point =
  let iteration = genericTake (iterations + 1) $ iterate (\z -> z*z + point) 0
   in fromMaybe 0 $ findIndex (\(a :+ b) -> a*a + b*b > 4) iteration
