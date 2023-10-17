module Views.PauseMenu where

import State (GlobalState(..), MenuRoute (GameView), MenuRoute(StartMenu), GameState (..))
import Assets(Assets(Assets,pacFont, emuFont))
import FontContainer(FontContainer(..))
import Rendering(renderString,renderButton, rectangleHovered, Rectangle (Rectangle), defaultButton)
import Graphics.Gloss ( Picture, blue, red, white, pictures)
import Graphics.Gloss.Interface.IO.Game ( Event (..), Key (MouseButton), MouseButton (..) )
import Graphics.Gloss.Data.Point ()
import System.Exit (exitSuccess)
import Views.StartMenu (drawParticles, updateParticles)

continueButton :: Rectangle
continueButton = Rectangle (0,0) 400 100 10

saveButton :: Rectangle
saveButton = Rectangle (0,-140) 500 100 10

mainMenuButton :: Rectangle
mainMenuButton = Rectangle (0,-280) 500 100 10

renderPauseMenu :: GlobalState -> IO Picture
renderPauseMenu s = do
    title <- renderString (0,250) (xxl (pacFont (assets s))) blue "PAUSED"
    drawnContinuButton <- defaultButton continueButton (l (emuFont (assets s))) "Continue" (mousePos s)
    drawnSaveButton <- defaultButton saveButton (l (emuFont (assets s))) "Save game " (mousePos s)
    drawnMainMenuButton <- defaultButton mainMenuButton (l (emuFont (assets s))) "Main Menu" (mousePos s)
    return (pictures [drawParticles s,title,drawnContinuButton,drawnSaveButton,drawnMainMenuButton])


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