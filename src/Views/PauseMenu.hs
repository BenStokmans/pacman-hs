module Views.PauseMenu where

import State (GlobalState(..), MenuRoute (..), MenuRoute(StartMenu), GameState (..))
import Assets(Assets(Assets,pacFont, emuFont))
import FontContainer(FontContainer(..))
import Rendering(renderString,renderButton, rectangleHovered, Rectangle (Rectangle), defaultButton)
import Graphics.Gloss ( Picture, blue, red, white, pictures)
import Graphics.Gloss.Interface.IO.Game ( Event (..), Key (MouseButton), MouseButton (..), SpecialKey (KeyEsc) )
import Graphics.Gloss.Data.Point ()
import System.Exit (exitSuccess)
import Views.StartMenu (drawParticles, updateParticles)
import Graphics.Gloss.Interface.IO.Interact (Key(..))
import System.Directory (getCurrentDirectory)
import Data.Text (pack, unpack)
import System.FilePath ((</>))
import Graphics.UI.TinyFileDialogs (saveFileDialog)
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (when)

continueButton :: Rectangle
continueButton = Rectangle (0,0) 400 100 10

saveButton :: Rectangle
saveButton = Rectangle (0,-140) 500 100 10

mainMenuButton :: Rectangle
mainMenuButton = Rectangle (0,-280) 500 100 10

renderPauseMenu :: GlobalState -> IO Picture
renderPauseMenu gs = do
    title <- renderString (0,250) (xxl (pacFont (assets gs))) blue "PAUSED"

    let lEmu = l (emuFont (assets gs))
    let mPos = mousePos gs
    drawnContinuButton <- defaultButton continueButton lEmu "Continue" mPos
    drawnMainMenuButton <- defaultButton mainMenuButton lEmu "Main Menu" mPos

    let saveText = if lastRoute gs == GameView then "game" else "level"
    drawnSaveButton <- defaultButton saveButton lEmu ("Save " ++ saveText) mPos

    return (pictures [drawParticles gs,title,drawnContinuButton,drawnSaveButton,drawnMainMenuButton])

saveEditorLevel :: GlobalState -> IO ()
saveEditorLevel s = do
    ws <- getCurrentDirectory
    file <- saveFileDialog (pack "select level") (pack $ ws </> "maps/newlevel.txt") [pack "*.txt"] (pack "level file")
    
    let fName = maybe "" unpack file
    when (isJust file) $ writeFile fName (show $ editorLevel s)

handleInputPauseMenu :: Event -> GlobalState -> IO GlobalState
handleInputPauseMenu (EventKey (SpecialKey KeyEsc) _ _ _) s = do return s {route = lastRoute s}
handleInputPauseMenu (EventKey (MouseButton LeftButton) _ _ _) s
    | continueButtonHover = do return s {route = lastRoute s}
    | saveButtonHover = do
        when (lastRoute s == EditorView) $ saveEditorLevel s
        return s                                 --TODO implement saving of gamestate
    | mainMenuButtonHover = do return s {route = StartMenu}
    where
        continueButtonHover = rectangleHovered (mousePos s) continueButton
        saveButtonHover = rectangleHovered (mousePos s) saveButton
        mainMenuButtonHover = rectangleHovered (mousePos s) mainMenuButton
handleInputPauseMenu _ s = do return s

handleUpdatePauseMenu :: Float -> GlobalState -> IO GlobalState
handleUpdatePauseMenu = updateParticles