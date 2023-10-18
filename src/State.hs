module State where

import Assets(Assets(Assets),loadAssets)
import Struct
import Graphics.Gloss (Point, Color, blue)
import Graphics.Gloss.Interface.IO.Game (Key (..), SpecialKey (..))
import Data.Map (Map, empty)

data Prompt = Prompt 
    {
        accentColor :: Color,
        promptText :: String,
        promptValue :: String,
        showTextField :: Bool,
        showCloseButton :: Bool,
        showConfirmButton :: Bool,
        blinkInterval :: Float,
        lastBlink :: Float,
        blink :: Bool,
        confirmAction :: GlobalState -> String -> GlobalState,
        closeAction :: GlobalState -> String -> GlobalState,
        darkenBackground :: Bool
    }

defaultPrompt :: Prompt
defaultPrompt = Prompt {
            accentColor = blue,
            promptText = "",
            promptValue = "",
            showCloseButton = True,
            showTextField = True,
            showConfirmButton = True,
            blinkInterval = 0.3,
            lastBlink = 0,
            blink = True,
            confirmAction = const,
            closeAction = const,
            darkenBackground = True
        }

data Settings = Settings 
    {
        windowSize :: (Float,Float),
        pacmanPadding :: Float,
        mazeMargin :: Float,
        lineThickness :: Float,
        enableDebugGrid :: Bool,
        editorGridDimensions :: Vec2
    }

data MenuRoute = StartMenu | GameView | EditorView | PauseMenu | GameOverMenu deriving (Eq,Show)
data GameStatus = Won | Lost | Playing | Paused deriving Eq
data GameState = GameState
    {
        lives :: Int,
        status :: GameStatus,
        prevClock :: Float,
        -- level :: GameLevel, -- (if we decide to include multiple level options)
        score :: Int,
        player :: Player, -- the player character for pacman
        pinky :: GhostActor, inky :: GhostActor, blinky :: GhostActor, clyde :: GhostActor -- the four ghost
    }

data GlobalState = GlobalState 
    {
        settings :: Settings,
        keys :: [Key],
        gameState :: GameState,
        route :: MenuRoute,
        assets :: Assets,
        mousePos :: Point,
        particles :: [(Point,Float)],
        prompt :: Maybe Prompt,
        clock :: Float
    }

initState :: IO GlobalState
initState = do 
    assets <- loadAssets "assets"
    return GlobalState {
        settings = Settings {
            windowSize = (800,800),
            pacmanPadding = 0.1,
            mazeMargin = 0.35,
            lineThickness = 15,
            enableDebugGrid = False,
            editorGridDimensions = Vec2 25 25
        },
        gameState = GameState {
            lives = 0,
            status = Paused,
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
        particles = [],
        keys = [],
        prompt = Nothing,
        clock = 0
    }