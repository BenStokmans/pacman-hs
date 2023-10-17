module Views.PauseMenu where

import State (GlobalState(..), MenuRoute (GameView), MenuRoute(StartMenu), GameState (..))
import Assets(Assets(Assets,pacFont, emuFont))
import FontContainer(FontContainer(..))
import Rendering(renderString,renderButton, rectangleHovered, Rectangle (Rectangle))
import Graphics.Gloss ( Picture, blue, red, white, pictures)
import Graphics.Gloss.Interface.IO.Game ( Event (..), Key (MouseButton), MouseButton (..) )
import Graphics.Gloss.Data.Point ()
import Particles (updateParticles, drawParticles)
import System.Exit (exitSuccess)

continueButton :: Rectangle
continueButton = Rectangle (0,0) 400 100 10

saveButton :: Rectangle
saveButton = Rectangle (0,-140) 500 100 10

mainMenuButton :: Rectangle
mainMenuButton = Rectangle (0,-280) 500 100 10

renderPauseMenu :: GlobalState -> IO Picture
renderPauseMenu s = do
    title <- renderString (0,250) (xxl (pacFont (assets s))) blue "PAUSED"
    drawnContinuButton <- renderButton continueButton (l (emuFont (assets s))) continueButtonButtonColor "Continue"
    drawnSaveButton <- renderButton saveButton (l (emuFont (assets s))) saveButtonColor "Save game "
    drawnMainMenuButton <- renderButton mainMenuButton (l (emuFont (assets s))) mainMenuButtonColor "Main Menu"
    return (pictures [drawParticles s,title,drawnContinuButton,drawnSaveButton,drawnMainMenuButton])
    where 
        continueButtonButtonColor = if rectangleHovered (mousePos s) continueButton then white else blue
        saveButtonColor = if rectangleHovered (mousePos s) saveButton then white else blue
        mainMenuButtonColor = if rectangleHovered (mousePos s) mainMenuButton then white else blue
        


handleInputPauseMenu :: Event -> GlobalState -> IO GlobalState
handleInputPauseMenu (EventKey (MouseButton LeftButton) b c _) s 
    | continueButtonHover = do return s {route = GameView}
    | saveButtonHover = do return s                                 --TODO implement saving
    | mainMenuButtonHover = do return s {route = StartMenu}
    where 
        continueButtonHover = rectangleHovered (mousePos s) continueButton
        saveButtonHover = rectangleHovered (mousePos s) saveButton
        mainMenuButtonHover = rectangleHovered (mousePos s) mainMenuButton
handleInputPauseMenu _ s = do return s

handleUpdatePauseMenu :: Float -> GlobalState -> IO GlobalState
handleUpdatePauseMenu add s = updateParticles add newState
    where
        g = gameState s
        c = clock g
        newState = s { gameState = g { clock = c + add } }