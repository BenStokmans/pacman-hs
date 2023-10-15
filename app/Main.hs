module Main where
  
import Pathfinding (getShortestPath)
import Rendering (renderString)
import Types ( GameStatus (Playing), GameState(GameState, lives, status, clock, level, score), Vec2(Vec2), Cell(Cell), CellType (Intersection), readLevel, setCells )
import Map ( calculateIntersections, calculateWallGroups, showMapWithWalls )
import Text.Printf
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import SDL.Font (Font, initialize, load)

-- start :: Vec2
-- start = Vec2 1 1
-- end :: Vec2
-- end = Vec2 21.0 9.0

-- main :: IO ()
-- main = do
--   level <- readLevel "assets/level.txt"
--   -- printf (show (setCells level (maybe [] (map (Cell Intersection)) (getShortestPath level start end))))
--   printf (showMapWithWalls level)

window :: Display
window = InWindow "Pacman" (800, 800) (10, 10)

background :: Color
background = black

initialState :: GameState
initialState = GameState {
        lives = 3,
        status = Playing,
        clock = 0,
        score = 0 
        }

render :: Font -> GameState -> IO Picture
render f _ = renderString f blue "PACMAN"

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (Char 'q') _ _ _) _ = do 
        error "quitting" -- this is very bad practice ? i think
handleInput _ game = do 
        return game

update :: Float -> GameState -> IO GameState
update seconds state = do 
        return state

main :: IO ()
main = do 
        initialize
        font <- load "assets/pacman.ttf" 100
        playIO window background 60 initialState (render font) handleInput update