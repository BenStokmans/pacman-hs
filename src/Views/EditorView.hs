module Views.EditorView where

import State (GlobalState(..), MenuRoute (..), MenuRoute(StartMenu), GameState (..))
import Assets(Assets(Assets,pacFont, emuFont))
import FontContainer(FontContainer(..))
import Rendering(renderString,renderButton, rectangleHovered, Rectangle (Rectangle), renderString')
import Graphics.Gloss
    ( Picture,
      blue,
      red,
      white,
      pictures,
      scale,
      rectangleSolid,
      translate,
      Picture(..) )
import Graphics.Gloss.Interface.IO.Game ( Event (..), Key (..), MouseButton (..), SpecialKey (..) )
import Graphics.Gloss.Data.Point ()
import System.Exit (exitSuccess)
import Views.StartMenu (drawParticles, updateParticles)
import Views.GameView (debugGrid, screenToGridPos, gridSizePx, cellSize, gridSize)
import Struct (Vec2(..))

wallIcon :: GlobalState -> Float -> Float -> IO Picture
wallIcon gs w h = do
    ((_,_),l) <- renderString' (l $ emuFont $ assets gs) white "W"
    return $ pictures [rectangleSolid w h, scale (w/64) (h/64) l ]

renderEditorView :: GlobalState -> IO Picture
renderEditorView s = do
    txt <- renderString (0,350) (m (emuFont (assets s))) red "Ooopsss nothing here yet...\n WIP"
    wi <- wallIcon s cw ch
    let hoveredCell = Color blue $ translate (x*cw-(w/2)+cw/2) (y*ch-(h/2)+ch/2) wi
    return (pictures [hoveredCell,debugGrid s,txt])
    where
        (Vec2 x y) = screenToGridPos s (mousePos  s)
        (w,h) = gridSizePx s
        (cw,ch) = cellSize (gridSize s) w h

handleInputEditorView :: Event -> GlobalState -> IO GlobalState
handleInputEditorView (EventKey (SpecialKey KeyEsc) _ _ _) s = do return s {route = PauseMenu}
handleInputEditorView _ s = do return s

handleUpdateEditorView :: Float -> GlobalState -> IO GlobalState
handleUpdateEditorView _ s = do return s