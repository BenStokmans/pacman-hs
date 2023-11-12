module Views.DebugSettingsMenu where

import Assets (Assets(..))
import FontContainer (FontContainer(m, xxl))
import GameLogic.MapLogic (tailNull)
import Graphics.Gloss (Picture, Point, blue, green, makeColor, pictures, red, white)
import Graphics.Gloss.Interface.IO.Game (Color, Event(EventKey), Key(MouseButton, SpecialKey), MouseButton(LeftButton), SpecialKey(KeyEsc))
import Rendering (Rectangle(..), completeButton, defaultButton, rectangleHovered, renderStringResize)
import SDL.Font (Font)
import State
  ( DebugSettings(enableGameText, enableGhostPath, enableGhostTarget,
              enableGhostText, enableGrid, enableHitboxes)
  , GlobalState(assets, history, mousePos, route, settings)
  , Settings(debugEnabled, debugSettings)
  )
import Views.StartMenu (drawParticles, updateParticles)

enableDebugButton :: Rectangle
enableDebugButton = Rectangle (0, 180) 400 50 7

enableGridButton :: Rectangle
enableGridButton = Rectangle (0, 110) 400 50 7

enableGhostPathButton :: Rectangle
enableGhostPathButton = Rectangle (0, 40) 400 50 7

enableGhostTextButton :: Rectangle
enableGhostTextButton = Rectangle (0, -30) 400 50 7

enableGhostTargetButton :: Rectangle
enableGhostTargetButton = Rectangle (0, -100) 400 50 7

enableHitboxesButton :: Rectangle
enableHitboxesButton = Rectangle (0, -170) 400 50 7

enableGameTextButton :: Rectangle
enableGameTextButton = Rectangle (0, -240) 400 50 7

saveButton :: Rectangle
saveButton = Rectangle (0, -330) 400 50 10

getSettingText :: Bool -> String -> String
getSettingText False s = "Enable " ++ s
getSettingText True s = "Disable " ++ s

gray :: Graphics.Gloss.Interface.IO.Game.Color
gray = makeColor 0.5 0.5 0.5 1

drawSettingsButton :: Bool -> Rectangle -> Font -> Bool -> String -> Point -> IO Picture
drawSettingsButton dis r f b s p =
  completeButton
    r
    f
    (getSettingText b s)
    p
    (if dis
       then gray
       else if b
              then green
              else red)
    (if dis
       then gray
       else white)

renderDebugMenu :: GlobalState -> IO Picture
renderDebugMenu gs = do
  title <- renderStringResize (0, 250) (xxl (pacFont (assets gs))) blue "DEBUG SETTINGS" 775 75
  let mEmu = m (emuFont (assets gs))
  let mPos = mousePos gs
  let sett = settings gs
  let debug = not $ debugEnabled sett
  let ds = debugSettings sett
  drawnEnableDebugButton <- drawSettingsButton False enableDebugButton mEmu (debugEnabled sett) "debug" mPos
  drawnEnableGridButton <- drawSettingsButton debug enableGridButton mEmu (enableGrid ds) "grid" mPos
  drawnEnableGhostPathButton <- drawSettingsButton debug enableGhostPathButton mEmu (enableGhostPath ds) "ghost path" mPos
  drawnEnableGhostTextButton <- drawSettingsButton debug enableGhostTextButton mEmu (enableGhostText ds) "ghost text" mPos
  drawnEnableGhostTargetButton <- drawSettingsButton debug enableGhostTargetButton mEmu (enableGhostTarget ds) "ghost target" mPos
  drawnEnableHitboxesButton <- drawSettingsButton debug enableHitboxesButton mEmu (enableHitboxes ds) "hitboxes" mPos
  drawnEnableGameTextButton <- drawSettingsButton debug enableGameTextButton mEmu (enableGameText ds) "game text" mPos
  drawnSaveButton <- defaultButton saveButton mEmu "Save" mPos
  return
    (pictures
       [ drawParticles gs
       , title
       , drawnEnableDebugButton
       , drawnEnableGridButton
       , drawnEnableGhostPathButton
       , drawnEnableGhostTextButton
       , drawnEnableGhostTargetButton
       , drawnEnableHitboxesButton
       , drawnEnableGameTextButton
       , drawnSaveButton
       ])

handleInputDebugMenu :: Event -> GlobalState -> IO GlobalState
handleInputDebugMenu (EventKey (SpecialKey KeyEsc) _ _ _) s = do
  return s {route = head (history s), history = tailNull (history s)}
handleInputDebugMenu (EventKey (MouseButton LeftButton) _ _ _) s
  | rectangleHovered mPos enableDebugButton = do return s {settings = sett {debugEnabled = not (debugEnabled sett)}}
  | rectangleHovered mPos saveButton = do return s {route = head his, history = tailNull his}
  | not (debugEnabled sett) = do return s
  | rectangleHovered mPos enableGridButton = do return s {settings = sett {debugSettings = ds {enableGrid = not (enableGrid ds)}}}
  | rectangleHovered mPos enableGhostPathButton = do return s {settings = sett {debugSettings = ds {enableGhostPath = not (enableGhostPath ds)}}}
  | rectangleHovered mPos enableGhostTextButton = do return s {settings = sett {debugSettings = ds {enableGhostText = not (enableGhostText ds)}}}
  | rectangleHovered mPos enableGhostTargetButton = do
    return s {settings = sett {debugSettings = ds {enableGhostTarget = not (enableGhostTarget ds)}}}
  | rectangleHovered mPos enableHitboxesButton = do return s {settings = sett {debugSettings = ds {enableHitboxes = not (enableHitboxes ds)}}}
  | rectangleHovered mPos enableGameTextButton = do return s {settings = sett {debugSettings = ds {enableGameText = not (enableGameText ds)}}}
  | otherwise = do return s
  where
    sett = settings s
    his = history s
    ds = debugSettings sett
    mPos = mousePos s
handleInputDebugMenu _ s = do
  return s

handleUpdateDebugMenu :: Float -> GlobalState -> IO GlobalState
handleUpdateDebugMenu = updateParticles
