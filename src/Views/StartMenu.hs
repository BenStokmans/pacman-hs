module Views.StartMenu where

import State (GlobalState(..), MenuRoute (GameView, EditorView), GameState (..), Settings (..))
import Assets(Assets(Assets,pacFont, emuFont), level)
import FontContainer(FontContainer(..))
import Rendering(renderString,renderButton, rectangleHovered, Rectangle (Rectangle), completeButton, defaultButton)
import Graphics.Gloss ( Picture (..), blue, red, white, pictures, makeColor, translate, circleSolid, black )
import Graphics.Gloss.Interface.IO.Game ( Event (..), Key (MouseButton), MouseButton (..) )
import Graphics.Gloss.Data.Point ()
import Control.Monad.Random (MonadRandom (getRandomR), Rand, RandomGen)
import System.Exit (exitSuccess)
import Struct (Player(pLocation))
import Map (getSpawnPoint)
import Views.GameView (gridToScreenPos)

startButton :: Rectangle
startButton = Rectangle (0,10) 500 100 10

quitButton :: Rectangle
quitButton = Rectangle (0,-150) 350 100 10

editorButton :: Rectangle
editorButton = Rectangle (0,-350) 250 50 10

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
                c = clock gs
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
    drawnEditorButton <- defaultButton editorButton (m (emuFont (assets s))) "Open editor" (mousePos s)
    return (pictures [drawParticles s,titleBg,title,subTitle,drawnStartButton,drawnQuitButton,drawnEditorButton])


handleInputStartMenu :: Event -> GlobalState -> IO GlobalState
handleInputStartMenu (EventKey (MouseButton LeftButton) b c _) s 
    | rectangleHovered (mousePos s) startButton = do return s {
        route = GameView,
        gameState = gs { player = ps { pLocation = spawnLoc } }
        }
    | rectangleHovered (mousePos s) editorButton = do return s { route = EditorView }
    | rectangleHovered (mousePos s) quitButton = do exitSuccess
    where 
        gs = gameState s
        ps = player gs
        spawnLoc = gridToScreenPos s (getSpawnPoint (level $ assets s))
handleInputStartMenu _ s = do return s

handleUpdateStartMenu :: Float -> GlobalState -> IO GlobalState
handleUpdateStartMenu add s = updateParticles add newState
    where
        g = gameState s
        c = clock g
        newState = s { gameState = g { clock = c + add } }