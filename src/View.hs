module View (ViewerState(..), MandelbrotView, getColour, getColours, initialiseMandelbrot, updateMandelbrot) where
import Data.Complex
import Data.Array.IO.Safe (IOUArray)
import Graphics.Gloss (Color, black, white)
import qualified Data.Array.MArray as A
import Data.Bool (bool)
import Lib (inMandelbrotSet)
import Control.Monad (forM_)
import Control.Exception (try, ArrayException)
import Data.Array (Array)
import Data.Array.IArray (amap)

data ViewerState = ViewerState
  { iterations :: Integer
  , zoom :: Integer
  , centre :: Complex Double
  , mandelbrotView :: MandelbrotView
  }

newtype MandelbrotView = MandelbrotView (IOUArray (Int, Int) Bool)

getColour :: MandelbrotView -> (Int, Int) -> IO Color
getColour (MandelbrotView arr) (x, y) = either (const black) (bool black white)
  <$> (try (A.readArray arr (x, y)) :: IO (Either ArrayException Bool))

getColours :: MandelbrotView -> IO (Array (Int, Int) Color)
getColours (MandelbrotView arr) = amap (bool white black) <$> A.freeze arr

initialiseMandelbrot :: Int -> Int -> IO MandelbrotView
initialiseMandelbrot width height = MandelbrotView 
  <$> A.newListArray ((0, 0), (width, height)) (repeat False)

updateMandelbrot :: ViewerState -> IO ()
updateMandelbrot state@ViewerState{mandelbrotView = MandelbrotView arr} = do
  ((minw,minh), (maxw,maxh)) <- A.getBounds arr
  forM_ [minw..maxw] $ \x ->
    forM_ [minh..maxh] $ \y ->
      let point = ((realToFrac (x - maxw `div` 2) :+ realToFrac (y - maxh `div` 2)) 
              / fromIntegral (2^zoom state :: Integer))
            + centre state
          b = inMandelbrotSet (iterations state) point
       in A.writeArray arr (x, y) b