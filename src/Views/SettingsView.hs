module Views.SettingsView where

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
import Rendering (Rectangle(Rectangle), defaultButton, rectangleHovered, renderButton, renderString, renderStringResize)
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

renderSettingsView :: GlobalState -> IO Picture
renderSettingsView gs = do
  title <- renderStringResize (0, 250) (xxl (pacFont (assets gs))) blue "SETTINGS" 775 120
  let lEmu = l (emuFont (assets gs))
  let mPos = mousePos gs
  drawnContinueButton <- defaultButton continueButton lEmu "Volume enz" mPos
  drawnMainMenuButton <- defaultButton mainMenuButton lEmu "checkbox" mPos
  return (pictures [drawParticles gs, title, drawnContinueButton, drawnMainMenuButton])

handleInputSettingsView :: Event -> GlobalState -> IO GlobalState
handleInputSettingsView (EventKey (SpecialKey KeyEsc) _ _ _) s = do
  return s {route = lastRoute s}
handleInputSettingsView _ s = do return s

handleUpdateSettingsView :: Float -> GlobalState -> IO GlobalState
handleUpdateSettingsView = updateParticles
