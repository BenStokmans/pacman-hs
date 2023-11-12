module Views.StartMenu where

import Assets (Assets(Assets, emuFont, pacFont, gearIconBlue, gearIconWhite, chartIconBlue, chartIconWhite))
import Control.Monad.Random (MonadRandom(getRandomR), Rand, RandomGen, when)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (pack, unpack)
import FontContainer (FontContainer(..))
import Graphics.Gloss (Picture(..), black, blue, circleSolid, makeColor, pictures, red, translate, white)
import Graphics.Gloss.Data.Point ()
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), SpecialKey(KeyEsc))
import Graphics.UI.TinyFileDialogs (openFileDialog, saveFileDialog)
import GameLogic.MapLogic
import Prompt (errorPrompt)
import Rendering
import State (GameState(..), GlobalState(..), MenuRoute(EditorView, GameView, StartMenu, SettingsView, LeaderBoardView), Prompt(..), Settings(..), defaultPrompt, gameGridInfo, emptyGameState, Player (..), GhostActor (..))
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import System.FilePath ((</>), takeBaseName)
import Text.Read (readMaybe)
import qualified SDL.Mixer as Mixer
import GameLogic.Pathfinding (getTraveledDirection)
import GameLogic.MapRendering
import GameLogic.Struct

selectMapButton :: Float -> Rectangle
selectMapButton w = Rectangle (0, 70) w 50 10

startButton :: Rectangle
startButton = Rectangle (0, -20) 500 100 10

quitButton :: Rectangle
quitButton = Rectangle (0, -180) 350 100 10

newMapButton :: Rectangle
newMapButton = Rectangle (-140, -350) 250 50 10

editMapButton :: Rectangle
editMapButton = Rectangle (150, -350) 300 50 10

settingsButton :: Rectangle
settingsButton = Rectangle (365, 365) 50 50 10

leaderBoardButton :: Rectangle
leaderBoardButton = Rectangle (305, 365) 50 50 10

drawParticles :: GlobalState -> Picture
drawParticles s = Color (makeColor 0 0 1 0.4) (pictures (map (\((x, y), r) -> translate x y (circleSolid r)) (particles s)))

animConst :: Float
animConst = 20

updateParticles :: Float -> GlobalState -> IO GlobalState
updateParticles f s
  | c - p < 0.4 = do return s {particles = map (\((xp, yp), r) -> ((xp, yp + (f * animConst)), r)) ps}
  | otherwise = do
    xp <- getRandomR (-x2 + x2 / 12, x2 - x2 / 12)
    r <- getRandomR (2 :: Float, 8)
    return s {gameState = gs {prevClock = c}, particles = ((xp, -y2 - (r / 2)), r) : ps}
  where
    gs = gameState s
    c = clock s
    p = prevClock gs
    (x, y) = windowSize (settings s)
    (x2, y2) = (x / 2, y / 2)
    ps = filter (\((_, yp), r) -> yp - r / 2 <= y2) (particles s)

renderStartMenu :: GlobalState -> IO Picture
renderStartMenu s = do
  let ass = assets s
  let xxlEmu = xxl (pacFont ass)
  titleBg <- renderString (0, 250) xxlEmu black "pacman"
  title <- renderString (0, 250) xxlEmu blue "PACMAN"
  let lEmu = l (emuFont ass)
  drawnStartButton <- defaultButton startButton lEmu "Start new game" (mousePos s)
  drawnQuitButton <- defaultButton quitButton lEmu "Quit game" (mousePos s)
  let mEmu = m (emuFont ass)
  let selectText = "Choose map: " ++ gameLevelName s
  (w, _) <- stringSize mEmu selectText
  drawnSelectMapButton <- defaultButton (selectMapButton (w + 40)) mEmu selectText (mousePos s)
  subTitle <- renderString (0, 160) mEmu red "By Ben Stokmans and Geerten Helmers"
  drawnNewMapButton <- defaultButton newMapButton mEmu "Create new map" (mousePos s)
  drawnEditMapButton <- defaultButton editMapButton mEmu "Edit existing map" (mousePos s)
  let drawnSettingsButton = defaultButtonImg settingsButton (gearIconBlue ass) (gearIconWhite ass) (mousePos s)
  let drawLeaderBoardButton = defaultButtonImg leaderBoardButton (chartIconBlue ass) (chartIconWhite ass) (mousePos s)
  return
    (pictures
       [drawParticles s, titleBg, title, subTitle, drawnSelectMapButton, drawnStartButton, drawnQuitButton, drawnNewMapButton, drawnEditMapButton, drawnSettingsButton,drawLeaderBoardButton])

emptyMap :: Float -> Float -> LevelMap
emptyMap w h = LevelMap w h (fr : ors ++ [lr])
  where
    fr = map (\x -> Cell Wall (Vec2 (fromInteger x :: Float) 0)) [0 .. (round w - 1)]
    ors =
      map
        (\y ->
           concat [ [Cell Wall (Vec2 0 (fromInteger y :: Float))]
           , map (\x -> Cell Empty (Vec2 (fromInteger x :: Float) (fromInteger y :: Float))) [1 .. (round w - 2)]
           , [Cell Wall (Vec2 (w - 1) (fromInteger y :: Float))]
           ])
        [1 .. (round h - 2)]
    lr = map (\x -> Cell Wall (Vec2 (fromInteger x :: Float) (h - 1))) [0 .. (round w - 1)]

confirmHeightPrompt :: GlobalState -> String -> IO GlobalState
confirmHeightPrompt s v
  | isJust heightInt = do return $ let (Vec2 w _) = editorGridDimensions set in s { settings = set {editorGridDimensions = Vec2 w height}
          , editorLevel = emptyMap w height
          , prompt = Nothing
          , route = EditorView
          , cachedWalls = processWalls $ editorLevel s
          }
  | otherwise = do return s {prompt = errorPrompt $ "Invalid width: \n" ++ show v}
  where
    set = settings s
    heightInt = readMaybe v :: Maybe Int
    height = maybe 0 (\v -> fromIntegral v :: Float) heightInt

confirmWidthPrompt :: GlobalState -> String -> IO GlobalState
confirmWidthPrompt s v
  | isJust widthInt = do return $  let (Vec2 _ y) = editorGridDimensions set in s { settings = set {editorGridDimensions = Vec2 width y}
          , prompt =
              Just
                defaultPrompt
                  { accentColor = blue
                  , promptText = "Enter grid height:"
                  , promptValue = show (round y)
                  , confirmAction = confirmHeightPrompt
                  , closeAction = \state _ -> do return state {route = StartMenu, prompt = Nothing}
                  }
          }
  | otherwise = do return s {prompt = errorPrompt $ "Invalid height: \n" ++ show v}
  where
    set = settings s
    widthInt = readMaybe v :: Maybe Int
    width = maybe 0 (\v -> fromIntegral v :: Float) widthInt

selectMap :: IO (Maybe (LevelMap, String))
selectMap = do
  ws <- getCurrentDirectory
  mFiles <- openFileDialog (pack "select map") (pack $ ws </> "maps") [pack "*.txt"] (pack "map file") False
  let (lm, name)
        | Just files <- mFiles =
          let f = head $ map unpack files
           in (readLevel f, takeBaseName f)
        | otherwise = (do return $ LevelMap 0 0 [], "")
  m <- lm
  let ls
        | isJust mFiles = Just (m, name)
        | otherwise = Nothing
  return ls

startGame :: GlobalState -> GlobalState
startGame s = s
              { route = GameView
              , history = [StartMenu]
              , gameState =
                  gs
                    { player = ps {pLocation = gridToScreenPos (gameGridInfo s) playerSpawn, pDirection = playerDirection}
                    , blinky = (blinky gs) {gLocation = gridToScreenPos (gameGridInfo s) (getGhostSpawnPoint level Blinky)}
                    , pinky = (pinky gs) {gLocation = gridToScreenPos (gameGridInfo s) (getGhostSpawnPoint level Pinky)}
                    , inky = (inky gs) {gLocation = gridToScreenPos (gameGridInfo s) (getGhostSpawnPoint level Inky)}
                    , clyde = (clyde gs) {gLocation = gridToScreenPos (gameGridInfo s) (getGhostSpawnPoint level Clyde)}
                    , gMap = level
                    , pellets = pellets
                    , totalPelletCount = length pellets
                    }
              , cachedWalls = processWalls level
              }
  where
    level = gameLevel s
    gs = emptyGameState -- initialize new gamestate
    ps = player gs
    pellets = getCellsWithTypes [Pellet, PowerUp] level
    playerSpawn = getSpawnPoint level
    playerDirection = fromMaybe North $ headMaybe $ map (getTraveledDirection playerSpawn) $ filterLevelVec2s level (not . cellHasTypes [Wall,GhostWall]) $ adjacentVecs playerSpawn

handleInputStartMenu :: Event -> GlobalState -> IO GlobalState
handleInputStartMenu (EventKey (SpecialKey KeyEsc) _ _ _) _ = do
  exitSuccess
handleInputStartMenu (EventKey (MouseButton LeftButton) b c _) s = do
  let gs = gameState s
  let ps = player gs
  let selectText = "Choose map: " ++ gameLevelName s
  (w, _) <- stringSize (m (emuFont (assets s))) selectText
  let newState
        | rectangleHovered (mousePos s) (selectMapButton (w + 40)) = do
          mMap <- selectMap
          let ns
                | Just (m, name) <- mMap = if validateLevel m then s {gameLevel = m, gameLevelName = name} else s {prompt = errorPrompt "Invalid map!"}
                | otherwise = s {prompt = errorPrompt "Invalid map!"}
          return ns
        | rectangleHovered (mousePos s) startButton = do return $ startGame s
        | rectangleHovered (mousePos s) newMapButton = do
          return
            s
              { prompt =
                  Just
                    defaultPrompt
                      { promptText = "Enter grid width:"
                      , promptValue =
                          let (Vec2 x _) = editorGridDimensions $ settings s
                           in show (round x)
                      , confirmAction = confirmWidthPrompt
                      , closeAction = \state _ -> do return state {route = StartMenu, prompt = Nothing}
                      }
              }
        | rectangleHovered (mousePos s) editMapButton = do
          mMap <- selectMap
          let ns
                | Just (m@(LevelMap w h _), _) <- mMap = if validateLevel m then s {editorLevel = m, route = EditorView, settings = (settings s) {editorGridDimensions = Vec2 w h}} else s {prompt = errorPrompt "Invalid map!"}
                | otherwise = s {prompt = errorPrompt "Invalid map!"}
          return ns
        | rectangleHovered (mousePos s) settingsButton = do return s {route = SettingsView, history = [StartMenu]}
        | rectangleHovered (mousePos s) leaderBoardButton = do return s {route = LeaderBoardView, history = [StartMenu]}
        | rectangleHovered (mousePos s) quitButton = do exitSuccess
        | otherwise = do return s
  newState
handleInputStartMenu _ s = do
  return s

handleUpdateStartMenu :: Float -> GlobalState -> IO GlobalState
handleUpdateStartMenu = updateParticles
