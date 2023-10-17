module Router where

import State(GlobalState(..),MenuRoute(..), windowSize, GameState (clock), Settings (..))
import Views.StartMenu
    ( renderStartMenu, handleInputStartMenu, handleUpdateStartMenu )
import Views.PauseMenu
    ( renderPauseMenu, handleInputPauseMenu, handleUpdatePauseMenu )
import Graphics.Gloss ( Picture )
import Graphics.Gloss.Interface.IO.Game
    ( Picture, Key(Char), Event(EventMotion, EventKey, EventResize) )
import System.Exit (exitSuccess)
import Control.Exception (handle)
import Views.GameView (renderGameView, handleInputGameView, handleUpdateGameView)
import SDL.Audio (PlaybackState(Pause))
import Views.EditorView (renderEditorView, handleInputEditorView, handleUpdateEditorView)

handleRender :: GlobalState -> IO Picture
handleRender s@(GlobalState { route = StartMenu }) = renderStartMenu s
handleRender s@(GlobalState { route = GameView }) = renderGameView s
handleRender s@(GlobalState { route = EditorView }) = renderEditorView s
handleRender s@(GlobalState { route = PauseMenu }) = renderPauseMenu s
handleRender _ = error "Route not implemented"

handleInput :: Event -> GlobalState -> IO GlobalState
handleInput (EventResize (w, h)) s = do return s { settings = set { windowSize = (fromIntegral w :: Float, fromIntegral h :: Float) } }
        where
          set = settings s
handleInput (EventKey (Char 'q') _ _ _) _ = do exitSuccess
handleInput (EventMotion p) s = do return s { mousePos = p }
handleInput e@(EventKey {}) s = if c-lp>cd then do
    ns <- newState
    return ns{ lastKeyPress = c }
    else do return s
        where
            r = route s
            cd = keyCooldown $ settings s
            lp = lastKeyPress s
            c = clock $ gameState s
            newState | r == StartMenu = handleInputStartMenu e s
                     | r == GameView = handleInputGameView e s
                     | r == EditorView = handleInputEditorView e s
                     | r == PauseMenu = handleInputPauseMenu e s
                     | otherwise = error "Route not implemented"

handleUpdate :: Float -> GlobalState -> IO GlobalState
handleUpdate e s@(GlobalState { route = StartMenu }) = handleUpdateStartMenu e s
handleUpdate e s@(GlobalState { route = GameView }) = handleUpdateGameView e s
handleUpdate e s@(GlobalState { route = EditorView }) = handleUpdateEditorView e s
handleUpdate e s@(GlobalState { route = PauseMenu }) = handleUpdatePauseMenu e s
handleUpdate e _ = error "Route not implemented"