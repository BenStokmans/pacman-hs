module Main where
  
import Router(handleRender,handleInput,handleUpdate)
import Assets (Assets(Assets,pacFont), loadAssets)
import Pathfinding (getShortestPath)
import Rendering (renderString)
import State (GlobalState(..), GameStatus (..), GameState(..),initState, Settings (windowSize))
import Struct (Vec2(Vec2), Cell(Cell), CellType (Intersection), readLevel, setCells )
import Map ( calculateIntersections, calculateWallGroups )
import Text.Printf
import Graphics.Gloss ( black, Display(InWindow) )
import Graphics.Gloss.Interface.IO.Game ( playIO )

import qualified Graphics.UI.GLFW          as GLFW
import Graphics.UI.GLFW (WindowHint(..))

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
        GLFW.windowHint (WindowHint'Samples (Just 4)) -- enable multi sample buffer
        state <- initState
        playIO (window (settings state)) black 60 state handleRender handleInput handleUpdate