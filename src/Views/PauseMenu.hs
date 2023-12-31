module Views.PauseMenu where

import Assets (Assets(emuFont, gearIconBlue, gearIconWhite, pacFont))
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Text (pack, unpack)
import FontContainer (FontContainer(l, xxl))
import GameLogic.MapLogic (tailNull, validateLevel)
import Graphics.Gloss (Picture, blue, pictures)
import Graphics.Gloss.Interface.IO.Game
  ( Event(EventKey)
  , Key(MouseButton, SpecialKey)
  , MouseButton(LeftButton)
  , Picture
  , SpecialKey(KeyEsc)
  , blue
  , pictures
  )
import Graphics.UI.TinyFileDialogs (saveFileDialog)
import Prompt (errorPrompt)
import Rendering (Rectangle(..), defaultButton, defaultButtonImg, rectangleHovered, renderString)
import State
  ( GlobalState(assets, editorLevel, history, mousePos, prompt, route)
  , MenuRoute(EditorView, GameView, PauseMenu, SettingsView, StartMenu)
  , Prompt(closeAction)
  )
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Views.StartMenu (drawParticles, settingsButton, updateParticles)

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
        if head (history gs) == GameView
          then "undefined"
          else "map"
  drawnSaveButton <- defaultButton saveButton lEmu ("Save " ++ saveText) mPos
  let ass = assets gs
  let drawnSettingsButton = defaultButtonImg settingsButton (gearIconBlue ass) (gearIconWhite ass) (mousePos gs)
  return (pictures [drawParticles gs, title, drawnContinueButton, drawnSaveButton, drawnMainMenuButton, drawnSettingsButton])

saveEditorLevel :: GlobalState -> IO ()
saveEditorLevel s = do
  ws <- getCurrentDirectory
  file <- saveFileDialog (pack "save map") (pack $ ws </> "maps/newmap.txt") [pack "*.txt"] (pack "map file")
  let fName = maybe "" unpack file
  when (isJust file) $ writeFile fName (show $ editorLevel s)

handleInputPauseMenu :: Event -> GlobalState -> IO GlobalState
handleInputPauseMenu (EventKey (SpecialKey KeyEsc) _ _ _) s = do
  return s {route = head (history s), history = tailNull (history s)}
handleInputPauseMenu (EventKey (MouseButton LeftButton) _ _ _) s
  | continueButtonHover = do return s {route = head his, history = tailNull his}
  | saveButtonHover && head his == EditorView && not (validateLevel $ editorLevel s) = do
    return
      s
        { prompt =
            let Just p = errorPrompt "Invalid map!\nPlease place all \nspawn points for \nthe ghosts and pacman"
             in Just $
                p
                  { closeAction =
                      \state _ -> do
                        return state {route = PauseMenu, prompt = Nothing}
                  }
        }
  | saveButtonHover = do
    when (head his == EditorView) $ saveEditorLevel s
    return s --TODO implement saving of game state
  | mainMenuButtonHover = do return s {route = StartMenu}
  | rectangleHovered (mousePos s) settingsButton = do return s {route = SettingsView, history = PauseMenu : his}
  where
    continueButtonHover = rectangleHovered (mousePos s) continueButton
    saveButtonHover = rectangleHovered (mousePos s) saveButton
    mainMenuButtonHover = rectangleHovered (mousePos s) mainMenuButton
    his = history s
handleInputPauseMenu _ s = do
  return s

handleUpdatePauseMenu :: Float -> GlobalState -> IO GlobalState
handleUpdatePauseMenu = updateParticles
