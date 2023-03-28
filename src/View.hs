module View (ViewerState(..), MandelbrotView, getColour, getColours, initialiseMandelbrot, updateMandelbrot, zoomImage) where
import Data.Complex
import Data.Array.IO.Safe (IOUArray)
import Graphics.Gloss (Color, black, makeColor)
import qualified Data.Array.MArray as A
import Lib (inMandelbrotSet)
import Control.Monad (forM_)
import Control.Exception (try, ArrayException)
import Data.Array (Array, (!))
import Data.Array.IArray (amap)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently_, Async)

data ViewerState = ViewerState
  { iterations :: Integer
  , zoom :: Integer
  , centre :: Complex Double
  , updatingAsync :: Maybe (Async ())
  , mandelbrotView :: MandelbrotView
  }

newtype MandelbrotView = MandelbrotView (IOUArray (Int, Int) Int)

getColour :: MandelbrotView -> (Int, Int) -> IO Color
getColour (MandelbrotView arr) (x, y) = either (const black) iterationsToColour
  <$> (try (A.readArray arr (x, y)) :: IO (Either ArrayException Int))

getColours :: MandelbrotView -> IO (Array (Int, Int) Color)
getColours (MandelbrotView arr) = amap iterationsToColour <$> A.freeze arr

iterationsToColour :: Int -> Color
iterationsToColour i
  | i == 0 = black
  | otherwise = makeColor 
      (fromIntegral (i `triangle` 10) / 10 + 1)
      (fromIntegral (i `triangle` 20) / 20) 
      (fromIntegral (i `triangle` 30) / 30) 
      1

triangle :: Integral a => a -> a -> a
triangle x n = abs (x - 2*n*((x + n) `div` (2*n)))

initialiseMandelbrot :: Int -> Int -> IO MandelbrotView
initialiseMandelbrot width height = MandelbrotView
  <$> A.newListArray ((0, 0), (width, height)) (repeat 0)

updateMandelbrot :: ViewerState -> IO ()
updateMandelbrot state@ViewerState{mandelbrotView = MandelbrotView arr} = do
  ((minw,minh), (maxw,maxh)) <- A.getBounds arr
  capabilities <- getNumCapabilities
  let divisions = (round . sqrt . fromIntegral) capabilities
      widths = splits ((maxw-minw) `div` divisions) [minw..maxw]
      heights = splits ((maxh-minh) `div` divisions) [minh..maxh]
  forConcurrently_ [(xs,ys) | xs <- widths, ys <- heights] $ \(xs,ys) ->
    forM_ [(x,y) | x <- xs, y <- ys] $ \(x,y) ->
      let point = ((realToFrac (x - maxw `div` 2) :+ realToFrac (y - maxh `div` 2))
              / fromIntegral (2^zoom state :: Integer))
            + centre state
          i = inMandelbrotSet (iterations state) point
       in A.writeArray arr (x, y) i
  where
    splits :: Int -> [a] -> [[a]]
    splits n xs = case splitAt n xs of
      (ys, []) -> [ys]
      (ys, zs) -> ys : splits n zs

-- Provides a twice-zoomed-in view of the current image
-- while the new image is being calculated.
zoomImage :: (Int,Int) -> ViewerState -> IO ()
zoomImage (cornerX,cornerY) ViewerState{mandelbrotView = MandelbrotView arr} = do
  ((minw,minh), (maxw,maxh)) <- A.getBounds arr
  arr' <- A.freeze arr
  forM_ [(x,y) | x <- [0..maxw-minw], y <- [0..maxh-minh]] $ \(x,y) ->
    let x' = (x `div` 2) + minw + cornerX
        y' = (y `div` 2) + minh + cornerY
     in A.writeArray arr (x+minw,y+minh) (arr'!(x',y'))
