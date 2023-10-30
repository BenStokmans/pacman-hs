module Views.StartMenu where

import Assets (Assets(Assets, emuFont, pacFont))
import Control.Monad.Random (MonadRandom(getRandomR), Rand, RandomGen)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (pack, unpack)
import FontContainer (FontContainer(..))
import Graphics.Gloss (Picture(..), black, blue, circleSolid, makeColor, pictures, red, translate, white)
import Graphics.Gloss.Data.Point ()
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), SpecialKey(KeyEsc))
import Graphics.UI.TinyFileDialogs (openFileDialog, saveFileDialog)
import Map (getGhostSpawnPoint, getSpawnPoint, processWalls)
import Prompt (errorPrompt)
import Rendering (Rectangle(Rectangle), completeButton, defaultButton, gridToScreenPos, rectangleHovered, renderButton, renderString, stringSize)
import State (GameState(..), GlobalState(..), MenuRoute(EditorView, GameView, StartMenu), Prompt(..), Settings(..), defaultPrompt)
import Struct (Cell(..), CellType(..), GhostActor(..), GhostType(..), LevelMap(LevelMap), Player(pLocation), Vec2(..), readLevel)
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import System.FilePath ((</>), takeBaseName)
import Text.Read (readMaybe)
import Views.GameView (gameGridInfo)

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
  let xxlEmu = xxl (pacFont (assets s))
  titleBg <- renderString (0, 250) xxlEmu black "pacman"
  title <- renderString (0, 250) xxlEmu blue "PACMAN"
  let lEmu = l (emuFont (assets s))
  drawnStartButton <- defaultButton startButton lEmu "Start new game" (mousePos s)
  drawnQuitButton <- defaultButton quitButton lEmu "Quit game" (mousePos s)
  let mEmu = m (emuFont (assets s))
  let selectText = "Choose map: " ++ mapName (gameState s)
  (w, _) <- stringSize mEmu selectText
  drawnSelectMapButton <- defaultButton (selectMapButton (w + 40)) mEmu selectText (mousePos s)
  subTitle <- renderString (0, 160) mEmu red "By Ben Stokmans and Geerten Helmers"
  drawnNewMapButton <- defaultButton newMapButton mEmu "Create new map" (mousePos s)
  drawnEditMapButton <- defaultButton editMapButton mEmu "Edit existing map" (mousePos s)
  return
    (pictures
       [drawParticles s, titleBg, title, subTitle, drawnSelectMapButton, drawnStartButton, drawnQuitButton, drawnNewMapButton, drawnEditMapButton])

emptyMap :: Float -> Float -> LevelMap
emptyMap w h =
  LevelMap w h $ -- generate walls on the edges -- generate walls on the edges -- generate walls on the edges -- generate walls on the edges
   -- generate walls on the edges
   -- generate walls on the edges
   -- generate walls on the edges -- generate walls on the edges
   -- generate walls on the edges
  concatMap (\x -> [Cell Wall (Vec2 (fromInteger x :: Float) 0), Cell Wall (Vec2 (fromInteger x :: Float) (h - 1))]) [0 .. (round w - 1)] ++
  concatMap (\y -> [Cell Wall (Vec2 0 (fromInteger y :: Float)), Cell Wall (Vec2 (w - 1) (fromInteger y :: Float))]) [0 .. (round h - 1)]

confirmHeightPrompt :: GlobalState -> String -> GlobalState
confirmHeightPrompt s v
  | isJust heightInt =
    let (Vec2 w _) = editorGridDimensions set
     in s
          { settings = set {editorGridDimensions = Vec2 w height}
          , editorLevel = emptyMap w height
          , prompt = Nothing
          , route = EditorView
          , cachedWalls = processWalls $ editorLevel s
          }
  | otherwise = s {prompt = errorPrompt $ "Invalid width: \n" ++ show v}
  where
    set = settings s
    heightInt = readMaybe v :: Maybe Int
    height = maybe 0 (\v -> fromIntegral v :: Float) heightInt

confirmWidthPrompt :: GlobalState -> String -> GlobalState
confirmWidthPrompt s v
  | isJust widthInt =
    let (Vec2 _ y) = editorGridDimensions set
     in s
          { settings = set {editorGridDimensions = Vec2 width y}
          , prompt =
              Just
                defaultPrompt
                  { accentColor = blue
                  , promptText = "Enter grid height:"
                  , promptValue = show (round y)
                  , confirmAction = confirmHeightPrompt
                  , closeAction = \state _ -> state {route = StartMenu, prompt = Nothing}
                  }
          }
  | otherwise = s {prompt = errorPrompt $ "Invalid height: \n" ++ show v}
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

handleInputStartMenu :: Event -> GlobalState -> IO GlobalState
handleInputStartMenu (EventKey (SpecialKey KeyEsc) _ _ _) _ = do
  exitSuccess
handleInputStartMenu (EventKey (MouseButton LeftButton) b c _) s = do
  let gs = gameState s
  let ps = player gs
  let selectText = "Choose map: " ++ mapName (gameState s)
  (w, _) <- stringSize (m (emuFont (assets s))) selectText
  let newState
        | rectangleHovered (mousePos s) (selectMapButton (w + 40)) = do
          mMap <- selectMap
          let ns
                | Just (m, name) <- mMap = s {gameState = gs {gMap = m, mapName = name}}
                | otherwise = s
          return ns
        | rectangleHovered (mousePos s) startButton = do
          return -- TODO: make this bit a seperate function, make sure all cases for direction etc are scaleable (start with pathfinding)
            s
              { route = GameView
              , gameState =
                  gs
                    { player = ps {pLocation = gridToScreenPos (gameGridInfo s) (getSpawnPoint (gMap gs))}
                    , blinky = (blinky gs) {gLocation = gridToScreenPos (gameGridInfo s) (getGhostSpawnPoint (gMap gs) Blinky)}
                    }
              , cachedWalls = processWalls $ gMap gs
              }
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
                      , closeAction = \state _ -> state {route = StartMenu, prompt = Nothing}
                      }
              }
        | rectangleHovered (mousePos s) editMapButton = do
          mMap <- selectMap
          let ns
                | Just (m@(LevelMap w h _), _) <- mMap =
                  s {editorLevel = m, route = EditorView, settings = (settings s) {editorGridDimensions = Vec2 w h}}
                | otherwise = s
          return ns
        | rectangleHovered (mousePos s) quitButton = do exitSuccess
        | otherwise = do return s
  newState
handleInputStartMenu _ s = do
  return s

handleUpdateStartMenu :: Float -> GlobalState -> IO GlobalState
handleUpdateStartMenu = updateParticles
