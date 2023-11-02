module Main where

import Graphics.Gloss (Display(..), black)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.UI.GLFW (WindowHint(..))
import qualified Graphics.UI.GLFW as GLFW
import Router (handleInput, handleRender, handleUpdate)
import State (GlobalState(..), Settings(..), initState)

window :: Settings -> Display
window s =
  let (w, h) = windowSize s
   in InWindow "Pacman" (round w, round h) (10, 10)

main :: IO ()
main = do
  GLFW.windowHint (WindowHint'Samples 4) -- enable multi sample buffer
  GLFW.windowHint (WindowHint'Resizable False) -- disable window resizing
  !state <- initState
  playIO (window (settings state)) black 60 state handleRender handleInput handleUpdate
