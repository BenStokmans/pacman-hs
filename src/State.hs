{-# OPTIONS_GHC -Wno-missing-fields #-}
module State where

import Assets(Assets(Assets),loadAssets)
import Struct
import Graphics.Gloss (Point)

data Settings = Settings 
    {
        windowSize :: (Float,Float),
        pacmanPadding :: Float,
        mazeMargin :: Float,
        lineThickness :: Float
    }

data MenuRoute = StartMenu | GameView | EditorView | PauseMenu | GameOverMenu deriving (Eq,Show)
data GameStatus = Won | Lost | Playing | Paused deriving Eq
data GameState = GameState
    {
        lives :: Int,
        status :: GameStatus,
        clock :: Float, -- the game time used for animation and ghost ai
        prevClock :: Float,
        -- level :: GameLevel, -- (if we decide to include multiple level options)
        score :: Int,
        player :: Player, -- the player character for pacman
        pinky :: GhostActor, inky :: GhostActor, blinky :: GhostActor, clyde :: GhostActor -- the four ghost
    }

data GlobalState = GlobalState 
    {
        settings :: Settings,
        gameState :: GameState,
        route :: MenuRoute,
        assets :: Assets,
        mousePos :: Point,
        particles :: [(Point,Float)]
    }

initState :: IO GlobalState
initState = do 
    assets <- loadAssets "assets"
    return GlobalState {
        settings = Settings {
            windowSize = (800,800),
            pacmanPadding = 0.1,
            mazeMargin = 0.35,
            lineThickness = 15
        },
        gameState = GameState {
            lives = 0,
            status = Paused,
            clock = 0,
            prevClock = 0,
            player = Player {
                pVelocity = 0,
                pDirection = North,
                pLocation = (0, 0),
                pFrame = 0,
                pBufferedInput = Nothing
            }
            -- todo init ghosts
        },
        route = StartMenu,
        assets = assets,
        mousePos = (0,0),
        particles = []
    }