{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Graphics.Gloss.Interface.IO.Game
import View
import Codec.Picture.Types (PixelRGBA8(..), generateImage)
import Data.Array ((!))
import Graphics.Gloss.Juicy (fromImageRGBA8)
import Data.Complex
import Control.Concurrent (forkIO, killThread)
import Data.Maybe (fromJust, isJust)
import Control.Monad (when)

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 800

background :: Color
background = black

window :: Display
window = InWindow "mandelbrot" (windowWidth, windowHeight) (0, 0)

initialState :: MandelbrotView -> ViewerState
initialState = ViewerState 50 6 0 Nothing

main :: IO ()
main = do
  mandelbrotView <- initialiseMandelbrot windowWidth windowHeight
  let state = initialState mandelbrotView
  updateMandelbrot state
  playIO window background 1 state getMandelbrot handleInput (const pure)

handleInput :: Event -> ViewerState -> IO ViewerState
handleInput (EventKey (MouseButton LeftButton) Down _ (x,y)) state = do
  when (isJust (updatingThread state)) $ do
    killThread (fromJust (updatingThread state))
  threadId <- forkIO $ updateMandelbrot state'
  pure state' {updatingThread = Just threadId}
  where 
    state' = state 
      { centre = centre state + ((realToFrac x :+ realToFrac (-y)) / fromIntegral (2^(zoom state + 1) :: Integer))
      , zoom = zoom state + 1
      }
handleInput (EventKey (MouseButton RightButton) Down _ (x,y)) state = do
  when (isJust (updatingThread state)) $ do
    killThread (fromJust (updatingThread state))
  threadId <- forkIO $ updateMandelbrot state'
  pure state' {updatingThread = Just threadId}
  where 
    state' = state 
      { centre = centre state - ((realToFrac x :+ realToFrac (-y)) / fromIntegral (2^zoom state :: Integer))
      , zoom = zoom state - 1
      }
handleInput (EventKey (Char 'z') Down _ _) state = do
  when (isJust (updatingThread state)) $ do
    killThread (fromJust (updatingThread state))
  threadId <- forkIO $ updateMandelbrot state'
  pure state' {updatingThread = Just threadId}
  where 
    state' = state {iterations = iterations state + 1}
handleInput (EventKey (Char 'x') Down _ _) state = do
  when (isJust (updatingThread state)) $ do
    killThread (fromJust (updatingThread state))
  threadId <- forkIO $ updateMandelbrot state'
  pure state' {updatingThread = Just threadId}
  where 
    state' = state {iterations = iterations state - 1}
handleInput _ state = pure state

getMandelbrot :: ViewerState -> IO Picture
getMandelbrot ViewerState{mandelbrotView} = do
  pixels <- getColours mandelbrotView
  let image = generateImage (curry (colourToRGBA8 . (pixels !))) windowWidth windowHeight
  pure (fromImageRGBA8 image)
  where
    colourToRGBA8 colour = let (r,g,b,a) = rgbaOfColor colour in PixelRGBA8 (round (r*255)) (round (g*255)) (round (b*255)) (round (a*255))


