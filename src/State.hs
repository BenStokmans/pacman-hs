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


data DebugSettings = DebugSettings {
  enableGrid :: Bool,
  enableGhostPath :: Bool,
  enableGhostText :: Bool,
  enableGhostTarget :: Bool,
  enableHitboxes :: Bool,
  enableGameText :: Bool
}

data Settings = Settings
  { windowSize :: (Float, Float)
  , debugEnabled :: Bool
  , debugSettings :: DebugSettings
  , musicEnabled :: Bool
  , fruitPadding :: Float
  , ghostPadding :: Float
  , pacmanPadding :: Float
  , mazeMargin :: Float
  , lineThickness :: Float
  , globalSpeedScalar :: Float
  , editorGridDimensions :: Vec2
  , ghostRespawnTimer :: Float
  , collisionLeniency :: Float -- an arbitrary value by which to reduce hitbox size
  , ghostStuckTimeout :: Float
  , ghostBlinkLength :: Float
  }

data MenuRoute
  = StartMenu
  | GameView
  | EditorView
  | PauseMenu
  | GameOverMenu
  | SettingsView
  | DebugSettingsMenu
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
  , godMode :: Bool
  , killingSpree :: Int
  , ghostKillAnimationTimer :: Float
  , status :: GameStatus
  , prevClock :: Float
  , gMap :: LevelMap
  , pellets :: [Cell]
  , score :: Int
  , fruitEaten :: Bool
  , pelletCount :: Int
  , totalPelletCount :: Int
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
  , history :: [MenuRoute]
  , gameLevelName :: String
  , gameLevel :: LevelMap
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
gameGridDimensions GlobalState {gameLevel = (LevelMap w h _)} = (w, h)

gameGridInfo :: GlobalState -> GridInfo
gameGridInfo gs =
  let (x, y) = gameGridDimensions gs
   in ((x, y), gridSizePx (x, y) gs)

ghostToSprite :: GlobalState -> GhostActor -> Picture
ghostToSprite gs ghost | gCurrentBehaviour ghost == Frightened = blueGhostSprite $ assets gs
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
    , godMode = False
    , pelletCount = 0
    , totalPelletCount = 0
    , ghostKillAnimationTimer = 0
    , lives = 3
    , fruitEaten = False
    , killingSpree = 0
    , level = 1
    , status = Paused
    , prevClock = 0
    , pellets = []
    , gMap = LevelMap 0 0 []
    , player = Player {pVelocity = 0.8, pDirection = East, pMoving = False, pLocation = (0, 0), pFrame = 0, pBufferedInput = Nothing}
    , blinky =
        GhostActor
          { ghostType = Blinky
          , gVelocity = 0
          , gRespawnTimer = 0
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gModeClock = 0
          , gFrightenedClock = 0
          , gAnimClock = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    , pinky =
        GhostActor
          { ghostType = Pinky
          , gVelocity = 0
          , gRespawnTimer = 0
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gModeClock = 0
          , gFrightenedClock = 0
          , gAnimClock = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    , inky =
        GhostActor
          { ghostType = Inky
          , gVelocity = 0
          , gRespawnTimer = 0
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gModeClock = 0
          , gFrightenedClock = 0
          , gAnimClock = 0
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    , clyde =
        GhostActor
          { ghostType = Clyde
          , gVelocity = 0
          , gRespawnTimer = 0
          , gDirection = North
          , gLocation = (-1000, -1000)
          , gTarget = Vec2 0 0
          , lastDirChange = Vec2 0 0
          , gModeClock = 0
          , gFrightenedClock = 0
          , gAnimClock = 0
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
            , debugSettings = DebugSettings {
                enableGrid = False
              , enableGhostPath = False
              , enableGhostText = False
              , enableGhostTarget = True
              , enableHitboxes = True
              , enableGameText = True
            }
            , globalSpeedScalar = 1.0
            , musicEnabled = False
            , ghostPadding = 0.20
            , pacmanPadding = 0.15
            , fruitPadding = 0.10
            , mazeMargin = 0.35
            , lineThickness = 15
            , editorGridDimensions = Vec2 25 25
            , ghostRespawnTimer = 2
            , collisionLeniency = 0.2
            , ghostStuckTimeout = 2
            , ghostBlinkLength = 0.5
            }
      , gameState = emptyGameState
      , gameLevelName = "default"
      , gameLevel = gMap
      , editorLevel = LevelMap 25 25 []
      , cachedWalls = []
      , route = StartMenu
      , history = []
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
