module Views.EditorView where

import State (GlobalState(..), MenuRoute (..), MenuRoute(StartMenu), GameState (..))
import Assets(Assets(Assets,pacFont, emuFont))
import FontContainer(FontContainer(..))
import Rendering(renderString,renderButton, rectangleHovered, Rectangle (Rectangle))
import Graphics.Gloss ( Picture, blue, red, white, pictures, scale)
import Graphics.Gloss.Interface.IO.Game ( Event (..), Key (..), MouseButton (..), SpecialKey (..) )
import Graphics.Gloss.Data.Point ()
import System.Exit (exitSuccess)
import Views.StartMenu (drawParticles, updateParticles)
import Views.GameView (debugGrid)


renderEditorView :: GlobalState -> IO Picture
renderEditorView s = do
    txt <- renderString (0,350) (m (emuFont (assets s))) red "Ooopsss nothing here yet...\n WIP"
    return (pictures [debugGrid s,txt]) 

handleInputEditorView :: Event -> GlobalState -> IO GlobalState
handleInputEditorView (EventKey (SpecialKey KeyEsc) _ _ _) s = do return s {route = PauseMenu}
handleInputEditorView _ s = do return s

handleUpdateEditorView :: Float -> GlobalState -> IO GlobalState
handleUpdateEditorView add s = updateParticles add newState
    where
        g = gameState s
        c = clock g
        newState = s { gameState = g { clock = c + add } }