module Main where

import           Assets                           (Assets (Assets, pacFont),
                                                   loadAssets)
import           Graphics.Gloss                   (Display (InWindow), black)
import           Graphics.Gloss.Interface.IO.Game (playIO)
import           Map                              (calculateIntersections)
import           Pathfinding                      (getShortestPath)
import           Rendering                        (renderString)
import           Router                           (handleInput, handleRender,
                                                   handleUpdate)
import           State                            (GameState (..),
                                                   GameStatus (..),
                                                   GlobalState (..),
                                                   Settings (windowSize),
                                                   initState)
import           Struct                           (Cell (Cell),
                                                   CellType (Intersection),
                                                   Vec2 (Vec2), readLevel,
                                                   setCells)
import           Text.Printf

import           Graphics.UI.GLFW                 (WindowHint (..))
import qualified Graphics.UI.GLFW                 as GLFW

-- start :: Vec2
-- start = Vec2 1 1
-- end :: Vec2
-- end = Vec2 21.0 9.0

-- main :: IO ()
-- main = do
--   level <- readLevel "assets/level.txt"
--   -- printf (show (setCells level (maybe [] (map (Cell Intersection)) (getShortestPath level start end))))
--   printf (showMapWithWalls level)

window :: Settings -> Display
window s = let (w,h) = windowSize s in InWindow "Pacman" (round w,round h) (10, 10)

main :: IO ()
main = do
        GLFW.windowHint (WindowHint'Samples 4) -- enable multi sample buffer
        state <- initState
        playIO (window (settings state)) black 60 state handleRender handleInput handleUpdate
