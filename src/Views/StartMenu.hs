module Views.StartMenu where

import State (GlobalState(..), MenuRoute (GameView, EditorView, StartMenu), GameState (..), Settings (..), Prompt (..), defaultPrompt)
import Assets(Assets(Assets,pacFont, emuFont), level)
import FontContainer(FontContainer(..))
import Rendering(renderString,renderButton, rectangleHovered, Rectangle (Rectangle), completeButton, defaultButton)
import Graphics.Gloss ( Picture (..), blue, red, white, pictures, makeColor, translate, circleSolid, black )
import Graphics.Gloss.Interface.IO.Game ( Event (..), Key (..), MouseButton (..), SpecialKey (KeyEsc) )
import Graphics.Gloss.Data.Point ()
import Control.Monad.Random (MonadRandom (getRandomR), Rand, RandomGen)
import System.Exit (exitSuccess)
import Struct (Player(pLocation), Vec2 (..), LevelMap (LevelMap), CellType (..), Cell (..), readLevel)
import Map (getSpawnPoint)
import Views.GameView (gridToScreenPos)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromMaybe)
import Prompt (errorPrompt)
import System.Directory (getCurrentDirectory)
import Graphics.UI.TinyFileDialogs (saveFileDialog, openFileDialog)
import Data.Text (pack, unpack)

startButton :: Rectangle
startButton = Rectangle (0,10) 500 100 10

quitButton :: Rectangle
quitButton = Rectangle (0,-150) 350 100 10

newMapButton :: Rectangle
newMapButton = Rectangle (-140,-350) 250 50 10

editMapButton :: Rectangle
editMapButton = Rectangle (150,-350) 300 50 10

drawParticles :: GlobalState -> Picture
drawParticles s = Color (makeColor 0 0 1 0.4) (pictures (map (\((x,y), r) -> translate x y (circleSolid r)) (particles s)))

animConst :: Float
animConst = 20

updateParticles :: Float -> GlobalState -> IO GlobalState
updateParticles f s | c-p < 0.4 = do return s { particles = map (\((xp,yp),r) -> ((xp,yp+(f*animConst)),r)) ps }
                    | otherwise = do
                      xp <- getRandomR (-x2+x2/12, x2-x2/12)
                      r <- getRandomR (2 :: Float, 8)
                      return s { gameState = gs {prevClock = c}, particles = ((xp,-y2-(r/2)),r):ps }
            where
                gs = gameState s
                c = clock s
                p = prevClock gs
                (x,y) = windowSize (settings s)
                (x2,y2) = (x/2,y/2)
                ps = filter (\((_,yp),r) -> yp-r/2 <= y2) (particles s)

renderStartMenu :: GlobalState -> IO Picture
renderStartMenu s = do
    titleBg <- renderString (0,250) (xxl (pacFont (assets s))) black "pacman"
    title <- renderString (0,250) (xxl (pacFont (assets s))) blue "PACMAN"
    subTitle <- renderString (0,160) (m (emuFont (assets s))) red "By Ben Stokmans and Geerten Helmers"
    drawnStartButton <- defaultButton startButton (l (emuFont (assets s))) "Start new game" (mousePos s)
    drawnQuitButton <- defaultButton quitButton (l (emuFont (assets s))) "Quit game" (mousePos s)
    drawnNewMapButton <- defaultButton newMapButton (m (emuFont (assets s))) "Create new map" (mousePos s)
    drawnEditMapButton <- defaultButton editMapButton (m (emuFont (assets s))) "Edit existing map" (mousePos s)
    return (pictures [drawParticles s,titleBg,title,subTitle,drawnStartButton,drawnQuitButton,drawnNewMapButton,drawnEditMapButton])

emptyMap :: Float -> Float -> LevelMap
emptyMap w h = LevelMap w h $ -- generate walls on the edges
                    concatMap (\x -> [Cell Wall (Vec2 (fromInteger x :: Float) 0),Cell Wall (Vec2 (fromInteger x :: Float) (h-1))]) [0..(round w-1)] ++
                    concatMap (\y -> [Cell Wall (Vec2 0 (fromInteger y :: Float)),Cell Wall (Vec2 (w-1) (fromInteger y :: Float))]) [0..(round h-1)]

confirmHeightPrompt :: GlobalState -> String -> GlobalState
confirmHeightPrompt s v | isJust heightInt =
                            let (Vec2 w _) = editorGridDimensions set in s {
                                settings = set { editorGridDimensions = Vec2 w height },
                                editorLevel = emptyMap w height,
                                prompt = Nothing,
                                route = EditorView
                                }
                       | otherwise = s { prompt = errorPrompt $ "Invalid width: \n" ++ show v }
        where
            set = settings s
            heightInt = readMaybe v :: Maybe Int
            height = maybe 0 (\v -> fromIntegral v :: Float) heightInt

confirmWidthPrompt :: GlobalState -> String -> GlobalState
confirmWidthPrompt s v | isJust widthInt =
                            let (Vec2 _ y) = editorGridDimensions set in s {
                                settings = set { editorGridDimensions = Vec2 width y },
                                prompt = Just defaultPrompt {
                                        accentColor = blue,
                                        promptText = "Enter grid height:",
                                        promptValue = show (round y),
                                        confirmAction = confirmHeightPrompt,
                                        closeAction = \state _ -> state { route = StartMenu, prompt = Nothing }
                                    }
                                }
                       | otherwise = s { prompt = errorPrompt $ "Invalid height: \n" ++ show v }
        where
            set = settings s
            widthInt = readMaybe v :: Maybe Int
            width = maybe 0 (\v -> fromIntegral v :: Float) widthInt

handleInputStartMenu :: Event -> GlobalState -> IO GlobalState
handleInputStartMenu (EventKey (SpecialKey KeyEsc) _ _ _) _ = do exitSuccess
handleInputStartMenu (EventKey (MouseButton LeftButton) b c _) s
    | rectangleHovered (mousePos s) startButton = do return s {
        route = GameView,
        gameState = gs { player = ps { pLocation = spawnLoc } }
        }
    | rectangleHovered (mousePos s) newMapButton = do return s { prompt = Just
        defaultPrompt {
            promptText = "Enter grid width:",
            promptValue = let (Vec2 x _ ) = editorGridDimensions $ settings s in show (round x),
            confirmAction = confirmWidthPrompt,
            closeAction = \state _ -> state { route = StartMenu, prompt = Nothing }
        } }
    | rectangleHovered (mousePos s) editMapButton = do
        ws <- getCurrentDirectory
        file <- openFileDialog (pack "select level") (pack ws) [pack "*.txt"] (pack "level file") False
        map@(LevelMap w h _) <- maybe (do return $ emptyMap 25 25) (readLevel . head) $ (Just . map unpack) =<< file
        return $ if isJust file then s { editorLevel = map, route = EditorView, settings = (settings s) { editorGridDimensions = Vec2 w h} } else s
    | rectangleHovered (mousePos s) quitButton = do exitSuccess
    where
        gs = gameState s
        ps = player gs
        spawnLoc = gridToScreenPos s (getSpawnPoint (level $ assets s))
handleInputStartMenu _ s = do return s

handleUpdateStartMenu :: Float -> GlobalState -> IO GlobalState
handleUpdateStartMenu = updateParticles