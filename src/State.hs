module State where

import Assets (Assets(..), loadAssets)
import Data.Aeson
import Data.Map (Map, empty)
import Data.Text hiding (map)
import GHC.Generics
import Graphics.Gloss (Color, Picture, Point, blue)
import Graphics.Gloss.Interface.IO.Game (Key(..), MouseButton, SpecialKey(..))
import Map (WallSection, getSpawnPoint, processWalls)
import Struct
import qualified SDL.Mixer as Mixer
import Control.Monad (unless)

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
  , debugEnabled :: Bool
  , musicEnabled :: Bool
  , ghostPadding :: Float
  , pacmanPadding :: Float
  , mazeMargin :: Float
  , lineThickness :: Float
  , editorGridDimensions :: Vec2
  , ghostRespawnTimer :: Float
  }

data MenuRoute
  = StartMenu
  | GameView
  | EditorView
  | PauseMenu
  | GameOverMenu
  | SettingsView
  deriving (Eq, Show)

data GameStatus
  = Won
  | Lost
  | Playing
  | Paused
  deriving (Eq, Show)

data GameState = GameState
  { lives :: Int
  , level :: Int
  , killingSpree :: Int
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

gridSizePx :: (Float, Float) -> GlobalState -> (Float, Float) -- grid size in pixels onscreen
gridSizePx (c, r) gs =
  let (x, y) = windowSize (settings gs)
   in (x * 0.8 * (c / r), y * 0.8 * (r / c))

gameGridDimensions :: GlobalState -> (Float, Float) -- grid size of map
gameGridDimensions GlobalState {gameState = GameState {gMap = (LevelMap w h _)}} = (w, h)

gameGridInfo :: GlobalState -> GridInfo
gameGridInfo gs =
  let (x, y) = gameGridDimensions gs
   in ((x, y), gridSizePx (x, y) gs)

ghostToSprite :: GlobalState -> GhostActor -> Picture
ghostToSprite gs ghost | gCurrentBehaviour ghost == Frightened && gVelocity ghost > 0 = blueGhostSprite $ assets gs
                       | ghostT == Blinky = blinkySprite $ assets gs
                       | ghostT == Pinky = pinkySprite $ assets gs
                       | ghostT == Inky = inkySprite $ assets gs
                       | ghostT == Clyde = clydeSprite $ assets gs
              where ghostT = ghostType ghost

getGhostActor :: GlobalState -> GhostType -> GhostActor
getGhostActor gs Blinky = blinky $ gameState gs
getGhostActor gs Pinky = pinky $ gameState gs
getGhostActor gs Inky = inky $ gameState gs
getGhostActor gs Clyde = clyde $ gameState gs

ghostActors :: GlobalState -> [GhostActor]
ghostActors gs = map (getGhostActor gs) [Blinky, Pinky, Inky, Clyde]

emptyGameState :: GameState
emptyGameState =
  GameState
    { score = 0
    , pelletCount = 0
    , lives = 3
    , killingSpree = 0
    , level = 1
    , status = Paused
    , prevClock = 0
    , mapName = "default"
    , gMap = LevelMap 0 0 []
    , player = Player {pVelocity = 80, pDirection = East, pMoving = False, pLocation = (0, 0), pFrame = 0, pBufferedInput = Nothing}
    , blinky =
        GhostActor
          { ghostType = Blinky
          , gVelocity = 75
          , gRespawnTimer = 0
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gModeClock = 0
          , gFrightenedClock = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    , pinky =
        GhostActor
          { ghostType = Pinky
          , gVelocity = 75
          , gRespawnTimer = 0
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gModeClock = 0
          , gFrightenedClock = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    , inky =
        GhostActor
          { ghostType = Inky
          , gVelocity = 75
          , gRespawnTimer = 0
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gModeClock = 0
          , gFrightenedClock = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    , clyde =
        GhostActor
          { ghostType = Clyde
          , gVelocity = 75
          , gRespawnTimer = 0
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gModeClock = 0
          , gFrightenedClock = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    }

initState :: IO GlobalState
initState = do
  assets <- loadAssets "assets"
  gMap <- readLevel "maps/default.txt"
  let gs = GlobalState { settings =
          Settings
            { windowSize = (800, 800)
            , debugEnabled = True
            , musicEnabled = False
            , ghostPadding = 0.20
            , pacmanPadding = 0.15
            , mazeMargin = 0.35
            , lineThickness = 15
            , editorGridDimensions = Vec2 25 25
            , ghostRespawnTimer = 2
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
  return gs
