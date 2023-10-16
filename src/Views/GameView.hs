module Views.GameView where
import State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), MouseButton (..), SpecialKey (KeyEsc))
import GHC.ResponseFile (escapeArgs)

gridSize :: Float
gridSize = 28

cellSize :: Float -> Float -> Float -> (Float,Float)
cellSize s w h = (w/s, h/s)

drawGrid :: Float -> Float -> Float -> Color -> Picture
drawGrid s w h col = Color col $ pictures $ concatMap (\i -> let hr = -h2 + hn*i in [Line [(-w2, hr),(w2,hr)], Line [(hr, -h2),(hr,h2)]]) [0..s]
    where
        w2 = w/2
        h2 = h/2
        (wn,hn) = cellSize s w h

debugGrid :: GlobalState -> Picture
debugGrid s = drawGrid gridSize w h green
    where 
        (x,y) = windowSize (settings s)
        (w,h) = (x*0.75,y*0.75)

drawPlayer :: GlobalState -> Picture
drawPlayer s = Color yellow (circleSolid (gridSize/2))

renderGameView :: GlobalState -> IO Picture
renderGameView s = do 
    return (pictures [debugGrid s, drawPlayer s])

handleInputGameView :: Event -> GlobalState -> IO GlobalState
handleInputGameView (EventKey (SpecialKey KeyEsc) _ _ _) s = do return s {route = PauseMenu}
handleInputGameView _ s = do return s

handleUpdateGameView :: Float -> GlobalState -> IO GlobalState
handleUpdateGameView _ s = do return s