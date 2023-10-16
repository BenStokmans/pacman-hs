module Views.StartMenu where

import State (GlobalState(..), MenuRoute (GameView), GameState (..))
import Assets(Assets(Assets,pacFont, emuFont))
import FontContainer(FontContainer(..))
import Rendering(renderString,renderButton, rectangleHovered, Rectangle (Rectangle))
import Graphics.Gloss ( Picture, blue, red, white, pictures )
import Graphics.Gloss.Interface.IO.Game ( Event (..), Key (MouseButton), MouseButton (..) )
import Graphics.Gloss.Data.Point ()
import Particles (updateParticles, drawParticles)
import System.Exit (exitSuccess)

startButton :: Rectangle
startButton = Rectangle (0,10) 500 100 10

quitButton :: Rectangle
quitButton = Rectangle (0,-150) 350 100 10

renderStartMenu :: GlobalState -> IO Picture
renderStartMenu s = do
    title <- renderString (0,250) (xxl (pacFont (assets s))) blue "PACMAN"
    subTitle <- renderString (0,160) (m (emuFont (assets s))) red "By Ben Stokmans and Geerten Helmers"
    drawnStartButton <- renderButton startButton (l (emuFont (assets s))) startButtonColor "Start new game"
    drawnQuitButton <- renderButton quitButton (l (emuFont (assets s))) quitButtonColor "Quit game "
    return (pictures [drawParticles s,title,subTitle,drawnStartButton,drawnQuitButton])
    where 
        startButtonColor = if rectangleHovered (mousePos s) startButton then white else blue
        quitButtonColor = if rectangleHovered (mousePos s) quitButton then white else blue


handleInputStartMenu :: Event -> GlobalState -> IO GlobalState
handleInputStartMenu (EventKey (MouseButton LeftButton) b c _) s 
    | startButtonHover = do return s {route = GameView}
    | quitButtonHover = do exitSuccess
    where 
        startButtonHover = rectangleHovered (mousePos s) startButton
        quitButtonHover = rectangleHovered (mousePos s) quitButton
handleInputStartMenu _ s = do return s

handleUpdateStartMenu :: Float -> GlobalState -> IO GlobalState
handleUpdateStartMenu add s = updateParticles add newState
    where
        g = gameState s
        c = clock g
        newState = s { gameState = g { clock = c + add } }