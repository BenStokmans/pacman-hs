module State where

import Assets (Assets(..), loadAssets)
import Data.Aeson (decode, decodeStrict)
import Data.Map (Map, empty)
import Data.Text hiding (empty, map)
import GHC.Generics
import Graphics.Gloss (Color, Picture, Point, blue)
import Graphics.Gloss.Interface.IO.Game (Key(..), MouseButton, SpecialKey(..))
import Map (WallSection, getSpawnPoint, processWalls)
import Struct
import qualified SDL.Mixer as Mixer
import Control.Monad (unless)
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Control.Exception.Base (try, catch)
import qualified Data.ByteString as Str
import Data.ByteString (ByteString)

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
  , confirmAction :: GlobalState -> String -> IO GlobalState
  , closeAction :: GlobalState -> String -> IO GlobalState
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
    , confirmAction = \x _ -> do return x
    , closeAction = \x _ -> do return x
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
  | LeaderBoardView
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
  , pauseGameTimer :: Float
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
  , lastClock :: Float
  , mouseDown :: Maybe MouseButton
  , cachedWalls :: [(Cell, WallSection)]
  , highScores :: Map String Int
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
ghostToSprite gs ghost | gCurrentBehaviour ghost == Frightened && not (gBlink ghost) = blueGhostSprite $ assets gs
                       | gCurrentBehaviour ghost == Frightened && gBlink ghost = whiteGhostSprite $ assets gs
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
    , pauseGameTimer = 0
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
          , gBlink = False
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
          , gBlink = False
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
          , gBlink = False
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
          , gBlink = False
          , gCurrentBehaviour = Scatter
          , lastModeChange = 0
          , gUpdate = 0
          }
    }

readHandler :: IOError -> IO ByteString
readHandler _ = return ""

readHighScores :: IO (Map String Int)
readHighScores = do
  text <- catch (Str.readFile "assets/highscores.json") readHandler
  return $ fromMaybe empty (decodeStrict text :: Maybe (Map String Int))

initState :: IO GlobalState
initState = do
  assets <- loadAssets "assets"
  gMap <- readLevel "maps/default.txt"
  highScores <- readHighScores
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
      , lastClock = 1
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
      , highScores = highScores
      }
  return gs
