module Views.PauseMenu where

import Assets (Assets(Assets, emuFont, pacFont))
import Control.Monad (when)
import Data.Aeson
import Data.Maybe (fromMaybe, isJust)
import Data.Text (pack, unpack)
import FontContainer (FontContainer(..))
import Graphics.Gloss (Picture, blue, pictures, red, white)
import Graphics.Gloss.Data.Point ()
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(MouseButton), MouseButton(..), SpecialKey(KeyEsc))
import Graphics.Gloss.Interface.IO.Interact (Key(..))
import Graphics.UI.TinyFileDialogs (saveFileDialog)
import Rendering (Rectangle(Rectangle), defaultButton, rectangleHovered, renderButton, renderString)
import State (GameState(..), GlobalState(..), MenuRoute(..))
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import Views.StartMenu (drawParticles, updateParticles)

continueButton :: Rectangle
continueButton = Rectangle (0, 0) 400 100 10

saveButton :: Rectangle
saveButton = Rectangle (0, -140) 500 100 10

mainMenuButton :: Rectangle
mainMenuButton = Rectangle (0, -280) 500 100 10

renderPauseMenu :: GlobalState -> IO Picture
renderPauseMenu gs = do
  title <- renderString (0, 250) (xxl (pacFont (assets gs))) blue "PAUSED"
  let lEmu = l (emuFont (assets gs))
  let mPos = mousePos gs
  drawnContinueButton <- defaultButton continueButton lEmu "Continue" mPos
  drawnMainMenuButton <- defaultButton mainMenuButton lEmu "Main Menu" mPos
  let saveText =
        if lastRoute gs == GameView
          then "game"
          else "map"
  drawnSaveButton <- defaultButton saveButton lEmu ("Save " ++ saveText) mPos
  return (pictures [drawParticles gs, title, drawnContinueButton, drawnSaveButton, drawnMainMenuButton])

saveEditorLevel :: GlobalState -> IO ()
saveEditorLevel s = do
  ws <- getCurrentDirectory
  file <- saveFileDialog (pack "save map") (pack $ ws </> "maps/newmap.txt") [pack "*.txt"] (pack "map file")
  let fName = maybe "" unpack file
  when (isJust file) $ writeFile fName (show $ editorLevel s)

saveGameState :: GlobalState -> IO ()
saveGameState s = do
  ws <- getCurrentDirectory
  file <- saveFileDialog (pack "save game") (pack $ ws </> "maps/game.txt") [pack "*.json"] (pack "game save")
  let fName = maybe "" unpack file
  when (isJust file) $ writeFile fName (show (encode $ gameState s))

handleInputPauseMenu :: Event -> GlobalState -> IO GlobalState
handleInputPauseMenu (EventKey (SpecialKey KeyEsc) _ _ _) s = do
  return s {route = lastRoute s}
handleInputPauseMenu (EventKey (MouseButton LeftButton) _ _ _) s
  | continueButtonHover = do return s {route = lastRoute s}
  | saveButtonHover = do
    when (lastRoute s == EditorView) $ saveEditorLevel s
    when (lastRoute s == GameView) $ saveGameState s
    return s --TODO implement saving of game state
  | mainMenuButtonHover = do return s {route = StartMenu}
  where
    continueButtonHover = rectangleHovered (mousePos s) continueButton
    saveButtonHover = rectangleHovered (mousePos s) saveButton
    mainMenuButtonHover = rectangleHovered (mousePos s) mainMenuButton
handleInputPauseMenu _ s = do
  return s

handleUpdatePauseMenu :: Float -> GlobalState -> IO GlobalState
handleUpdatePauseMenu = updateParticles
