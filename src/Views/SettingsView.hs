module Views.SettingsView where

import Assets (Assets(emuFont, pacFont))
import Control.Monad (unless, when)
import FontContainer (FontContainer(l, xxl))
import GameLogic.MapLogic (tailNull)
import Graphics.Gloss (Picture, blue, pictures)
import Graphics.Gloss.Interface.IO.Game (Event(EventKey), Key(MouseButton, SpecialKey), MouseButton(LeftButton), SpecialKey(KeyEsc))
import Rendering (Rectangle(..), defaultButton, rectangleHovered, renderStringResize)
import qualified SDL.Mixer as Mixer
import State (GlobalState(assets, history, mousePos, route, settings), MenuRoute(DebugSettingsMenu, SettingsView), Settings(musicEnabled))
import Views.StartMenu (drawParticles, updateParticles)

musicButton :: Rectangle
musicButton = Rectangle (0, 100) 500 100 10

debugButton :: Rectangle
debugButton = Rectangle (0, -50) 600 100 10

saveButton :: Rectangle
saveButton = Rectangle (0, -300) 400 100 10

renderSettingsView :: GlobalState -> IO Picture
renderSettingsView gs = do
  title <- renderStringResize (0, 250) (xxl (pacFont (assets gs))) blue "SETTINGS" 775 120
  let lEmu = l (emuFont (assets gs))
  let mPos = mousePos gs
  let sett = settings gs
  let musicText =
        if musicEnabled sett
          then "Disable Music"
          else "Enable Music"
  drawnMusicButton <- defaultButton musicButton lEmu musicText mPos
  drawnDebugButton <- defaultButton debugButton lEmu "Debug settings" mPos
  saveButton <- defaultButton saveButton lEmu "Save" mPos
  return (pictures [drawParticles gs, title, drawnMusicButton, drawnDebugButton, saveButton])

handleInputSettingsView :: Event -> GlobalState -> IO GlobalState
handleInputSettingsView (EventKey (SpecialKey KeyEsc) _ _ _) s = do
  return s {route = head (history s), history = tailNull (history s)}
handleInputSettingsView (EventKey (MouseButton LeftButton) _ _ _) s
  | rectangleHovered (mousePos s) saveButton = do return s {route = head his, history = tailNull his}
  | rectangleHovered (mousePos s) musicButton = do
    when (musicEnabled sett) $ Mixer.pause Mixer.AllChannels
    unless (musicEnabled sett) $ Mixer.resume Mixer.AllChannels
    return s {settings = sett {musicEnabled = not (musicEnabled sett)}}
  | rectangleHovered (mousePos s) debugButton = do return s {route = DebugSettingsMenu, history = SettingsView : his}
  where
    sett = settings s
    his = history s
handleInputSettingsView _ s = do
  return s

handleUpdateSettingsView :: Float -> GlobalState -> IO GlobalState
handleUpdateSettingsView = updateParticles
