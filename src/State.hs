{-# OPTIONS_GHC -Wno-missing-fields #-}
module State where

import Assets(Assets(Assets),loadAssets)
import Struct
import Graphics.Gloss (Point)

data Settings = Settings 
    {
        windowSize :: (Float,Float)
    }

data MenuRoute = StartMenu | GameView | PauseMenu | GameOverMenu deriving (Eq,Show)
data GameStatus = Won | Lost | Playing | Paused deriving Eq
data GameState = GameState
    {
        lives :: Int,
        status :: GameStatus,
        clock :: Float, -- the game time used for animation and ghost ai
        prevClock :: Float,
        level :: GameLevel, -- (if we decide to include multiple level options)
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
            windowSize = (800,800)
        },
        gameState = GameState {
            lives = 0,
            status = Paused,
            clock = 0,
            prevClock = 0,
            level = DefaultLevel,
            player = Player {
                pVelocity = 0,
                pDirection = North,
                pLocation = Vec2 0 0,
                pFrame = 0
            }
            -- todo init ghosts
        },
        route = GameView,
        assets = assets,
        mousePos = (0,0),
        particles = []
    }