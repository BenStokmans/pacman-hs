module State where

import Assets (Assets(Assets), loadAssets)
import Data.Aeson
import Data.Map (Map, empty)
import Data.Text
import GHC.Generics
import Graphics.Gloss (Color, Picture, Point, blue)
import Graphics.Gloss.Interface.IO.Game (Key(..), MouseButton, SpecialKey(..))
import Map (WallSection, calculateIntersections, getSpawnPoint, processWalls)
import Struct

data Prompt = Prompt
  { accentColor :: Color
  , promptText :: String
  , promptValue :: String
  , showTextField :: Bool
  , showCloseButton :: Bool
  , showConfirmButton :: Bool
  , blinkInterval :: Float
  , lastBlink :: Float
  , blink :: Bool
  , confirmAction :: GlobalState -> String -> GlobalState
  , closeAction :: GlobalState -> String -> GlobalState
  , darkenBackground :: Bool
  }

defaultPrompt :: Prompt
defaultPrompt =
  Prompt
    { accentColor = blue
    , promptText = ""
    , promptValue = ""
    , showCloseButton = True
    , showTextField = True
    , showConfirmButton = True
    , blinkInterval = 0.3
    , lastBlink = 0
    , blink = True
    , confirmAction = const
    , closeAction = const
    , darkenBackground = True
    }

data Settings = Settings
  { windowSize :: (Float, Float)
  , ghostPadding :: Float
  , pacmanPadding :: Float
  , mazeMargin :: Float
  , lineThickness :: Float
  , enableDebugGrid :: Bool
  , editorGridDimensions :: Vec2
  , ghostTagetCD :: Float
  }

data MenuRoute
  = StartMenu
  | GameView
  | EditorView
  | PauseMenu
  | GameOverMenu
  deriving (Eq, Show)

data GameStatus
  = Won
  | Lost
  | Playing
  | Paused
  deriving (Eq, Show)

data GameState = GameState
  { lives :: Int
  , status :: GameStatus
  , prevClock :: Float
  , mapName :: String
  , gMap :: LevelMap
  , score :: Int
  , pelletCount :: Int
  , player :: Player -- the player character for pacman
  , pinky :: GhostActor
  , inky :: GhostActor
  , blinky :: GhostActor
  , clyde :: GhostActor -- the four ghost
  } deriving (Generic, Show)

instance ToJSON GameState where
  toJSON (GameState {score = score, lives = lives}) = object ["score" .= score, "lives" .= lives]

data EditorTool
  = WallTool
  | SpawnTool
  | FoodTool
  | PowerUpTool
  | GhostTool
  | GhostWallTool
  deriving (Eq)

data GlobalState = GlobalState
  { settings :: Settings
  , keys :: [Graphics.Gloss.Interface.IO.Game.Key]
  , gameState :: GameState
  , route :: MenuRoute
  , assets :: Assets
  , mousePos :: Point
  , particles :: [(Point, Float)]
  , prompt :: Maybe Prompt
  , clock :: Float
  , lastRoute :: MenuRoute
  , editorLevel :: LevelMap
  , editorTool :: EditorTool
  , editorGhost :: GhostType
  , previewEditor :: Bool
  , mouseDown :: Maybe MouseButton
  , cachedWalls :: [(Cell, WallSection)]
  }

emptyGameState :: GameState
emptyGameState =
  GameState
    { score = 0
    , pelletCount = 0
    , lives = 0
    , status = Paused
    , prevClock = 0
    , mapName = "default"
    , gMap = LevelMap 0 0 []
    , player = Player {pVelocity = 80, pDirection = East, pMoving = False, pLocation = (0, 0), pFrame = 0, pBufferedInput = Nothing}
    , blinky =
        GhostActor
          { ghostType = Blinky
          , gVelocity = 75
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gBehaviourTimer = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    , pinky =
        GhostActor
          { ghostType = Pinky
          , gVelocity = 75
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gBehaviourTimer = 0
          , gCurrentBehaviour = Chase
          , lastModeChange = 0
          , gUpdate = 0
          }
    , inky =
        GhostActor
          { ghostType = Inky
          , gVelocity = 75
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gBehaviourTimer = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    , clyde =
        GhostActor
          { ghostType = Clyde
          , gVelocity = 75
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gBehaviourTimer = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    }

initState :: IO GlobalState
initState = do
  assets <- loadAssets "assets"
  gMap <- readLevel "maps/default.txt"
  return
    GlobalState
      { settings =
          Settings
            { windowSize = (800, 800)
            , ghostPadding = 0.20
            , pacmanPadding = 0.15
            , mazeMargin = 0.35
            , lineThickness = 15
            , enableDebugGrid = False
            , editorGridDimensions = Vec2 25 25
            , ghostTagetCD = 0.1
            }
      , gameState = emptyGameState {gMap = gMap}
      , editorLevel = LevelMap 25 25 []
      , cachedWalls = []
      , route = StartMenu
      , lastRoute = StartMenu
      , assets = assets
      , mousePos = (0, 0)
      , particles = []
      , keys = []
      , prompt = Nothing
      , clock = 0
      , editorTool = WallTool
      , editorGhost = Blinky
      , mouseDown = Nothing
      , previewEditor = False
      }
