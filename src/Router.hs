module Router where

import Control.Exception (handle)
import Data.List (delete)
import Data.Maybe
import Graphics.Gloss (Picture(..), blank, makeColor, pictures, rectangleSolid)
import Graphics.Gloss.Interface.IO.Game
  ( Event(EventKey, EventMotion, EventResize)
  , Key(..)
  , KeyState(..)
  , Modifiers(..)
  , Picture
  , SpecialKey(..)
  )
import Prompt (emptyPrompt, handleInputPrompt, handleUpdatePrompt, renderPrompt)
import SDL.Audio (PlaybackState(Pause))
import State
  ( GlobalState(..)
  , MenuRoute(..)
  , Prompt(..)
  , Settings(..)
  , windowSize
  )
import System.Exit (exitSuccess)
import Views.EditorView
  ( handleInputEditorView
  , handleUpdateEditorView
  , renderEditorView
  )
import Views.GameView
  ( gridSizePx
  , handleInputGameView
  , handleUpdateGameView
  , renderGameView
  )
import Views.PauseMenu
  ( handleInputPauseMenu
  , handleUpdatePauseMenu
  , renderPauseMenu
  )
import Views.StartMenu
  ( handleInputStartMenu
  , handleUpdateStartMenu
  , renderStartMenu
  )

handleRender :: GlobalState -> IO Picture
handleRender s@(GlobalState {route = r, prompt = p}) = do
  renderedMain <- image
  renderedPrompt <- pImage
  return $ pictures [renderedMain, curtain, renderedPrompt]
  where
    image
      | r == StartMenu = renderStartMenu s
      | r == GameView = renderGameView s
      | r == EditorView = renderEditorView s
      | r == PauseMenu = renderPauseMenu s
      | otherwise = error "Route not implemented"
    pImage
      | isJust p = renderPrompt s (fromMaybe emptyPrompt p)
      | otherwise = do return blank
    curtain
      | isJust p =
        if darkenBackground (fromMaybe emptyPrompt p)
          then Color (makeColor 0 0 0 0.4) $
               let (w, h) = windowSize $ settings s
                in rectangleSolid w h
          else blank
      | otherwise = blank

dummyEvent :: Event
dummyEvent = EventKey (SpecialKey KeyF25) Up (Modifiers {}) (0, 0)

handleInput :: Event -> GlobalState -> IO GlobalState
handleInput (EventKey (Char 'q') _ _ _) _ = do
  exitSuccess -- comment when not debugging
handleInput (EventResize (w, h)) s = do
  return
    s
      { settings =
          set {windowSize = (fromIntegral w :: Float, fromIntegral h :: Float)}
      }
  where
    set = settings s
handleInput (EventMotion p) s = do
  return s {mousePos = p}
handleInput e@(EventKey k Down _ _) s =
  if k `notElem` keys s
    then do
      let ps = promptState s
      ns <- newState ps
      return ns {keys = k : keys s}
    else do
      return s
  where
    r = route s
    (promptEvent, promptState)
      | isJust $ prompt s = (dummyEvent, handleInputPrompt e)
      | otherwise = (e, const s)
    newState
      | r == StartMenu = handleInputStartMenu promptEvent
      | r == GameView = handleInputGameView promptEvent
      | r == EditorView = handleInputEditorView promptEvent
      | r == PauseMenu = handleInputPauseMenu promptEvent
      | otherwise = error "Route not implemented"
handleInput e@(EventKey (SpecialKey KeyUnknown) Up _ _) s -- for some reason when the user lets go of a number key its reported as unknown
 = do
  return s {keys = filter (not . checkSpecialUnknown) $ keys s} -- so we have to implement this workaround
  where
    numberChars = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']
    checkSpecialUnknown k =
      case k of
        (Char c) -> c `elem` numberChars
        _ -> False
handleInput e@(EventKey k Up _ _) s = do
  return s {keys = delete k $ keys s}

handleUpdate :: Float -> GlobalState -> IO GlobalState
handleUpdate f s@(GlobalState {route = r, prompt = p}) = do
  pState <- promptState
  newState pState
  where
    intState = s {clock = clock s + f}
    promptState
      | isJust p = handleUpdatePrompt f intState (fromMaybe emptyPrompt p)
      | otherwise = do return intState
    newState
      | r == StartMenu = handleUpdateStartMenu f
      | r == GameView = handleUpdateGameView f
      | r == EditorView = handleUpdateEditorView f
      | r == PauseMenu = handleUpdatePauseMenu f
      | otherwise = error "Route not implemented"
