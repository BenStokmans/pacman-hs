module Views.GameView where
import State
    ( GlobalState(..),
      Settings(windowSize),
      GameState(clock, player, prevClock), MenuRoute (..) )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), MouseButton (..), SpecialKey (..))
import Assets (Assets(..), Anim (..), PacManSprite (..))
import Struct (LevelMap(..), Player(..), Direction (..), Cell(..), CellType(..), Vec2(..))
import Rendering (renderStringTopLeft)
import FontContainer (FontContainer(..))
import Map (wallSectionToPic)

gridSize :: GlobalState -> (Float,Float)
gridSize GlobalState { assets = Assets { level = (LevelMap w h _) }} = (w,h)

cellSize :: (Float,Float) -> Float -> Float -> (Float,Float)
cellSize (sx,sy) w h = (w/sx, h/sy)

drawGrid :: GlobalState -> Float -> Float -> Color -> Picture
drawGrid s w h col = Color col $ pictures $ map (\i -> let hc = -w2 + wn*i in Line [(hc, -h2),(hc,h2)]) [0..x] ++ map (\i -> let hr = -h2 + hn*i in Line [(-w2, hr),(w2,hr)]) [0..y]
    where
        w2 = w/2
        h2 = h/2
        (x,y) = gridSize s
        (wn,hn) = cellSize (x,y) w h

gridSizePx :: GlobalState -> (Float, Float)
gridSizePx s = (x*0.8*(c/r),y*0.8*(r / c))
    where
        (x,y) = windowSize (settings s)
        (r,c) = gridSize s

debugGrid :: GlobalState -> Picture
debugGrid s = let (w,h) = gridSizePx s in drawGrid s w h green

-- drawMap :: GlobalState -> Picture
-- drawMap s = Color blue $ pictures $ map (\(Cell _ (Vec2 x y)) -> translate (x*wn-w2+wn/2) (y*hn-h2+hn/2) (rectangleSolid wn hn)) walls
--     where
--         (LevelMap _ _ m) = level (assets s)
--         walls = filter (\(Cell t _) -> t == Wall) m
--         (w,h) = gridSizePx s
--         w2 = w/2
--         h2 = h/2
--         (wn,hn) = cellSize (gridSize s) w h

drawMap :: GlobalState -> Picture
drawMap s = Color blue $ pictures $ map (\(Cell _ (Vec2 x y),w) -> translate (x*wn-w2+wn/2) (y*hn-h2+hn/2) (wallSectionToPic wn hn w)) walls
    where
        walls = concat $ wallGroups $ assets s
        (w,h) = gridSizePx s
        w2 = w/2
        h2 = h/2
        (wn,hn) = cellSize (gridSize s) w h

getPlayerAnimation :: GlobalState -> Anim
getPlayerAnimation s | d == South = down as
                     | d == West = left as
                     | d == East = right as
                     | otherwise = up as
                where
                    d = pDirection $ player $ gameState s
                    as = pacSprite $ assets s

drawPlayer :: GlobalState -> Picture
drawPlayer s = Color yellow (scale scalar scalar (getPlayerAnimation s !! frame))
    where
        frame = pFrame $ player $ gameState s
        (wc,hc) = let (w,h) = gridSizePx s in cellSize (gridSize s) w h
        scalar = (wc/8)*0.8

renderGameView :: GlobalState -> IO Picture
renderGameView gs = do
    debugString <- renderStringTopLeft (s (emuFont (assets gs))) green
            (("Current pacman frame: " ++ show (pFrame $ player $ gameState gs)) ++
                (let (x,y) = gridSize gs in "\n Gridsize: (" ++ show x ++ ", " ++ show y ++ ")"))
    let debug = translate (-400) 400 debugString
    return (pictures [drawMap gs, drawPlayer gs, debug])

keyToDirection :: Direction -> Key -> Direction
keyToDirection _ (SpecialKey KeyUp) = North
keyToDirection _ (SpecialKey KeyDown) = South
keyToDirection _ (SpecialKey KeyLeft) = West
keyToDirection _ (SpecialKey KeyRight) = East
keyToDirection _ (Char 'w') = North
keyToDirection _ (Char 'a') = West
keyToDirection _ (Char 's') = South
keyToDirection _ (Char 'd') = East
keyToDirection d _ = d

handleInputGameView :: Event -> GlobalState -> IO GlobalState
handleInputGameView (EventKey (SpecialKey KeyEsc) _ _ _) s = do return s {route = PauseMenu}
handleInputGameView (EventKey k _ _ _) s = do return s { gameState = gs { player = ps { pDirection = keyToDirection (pDirection ps) k } } }
                        where
                            gs = gameState s
                            ps = player gs
handleInputGameView _ s = do return s

updatePlayerAnimState :: Float -> GlobalState -> IO GlobalState
updatePlayerAnimState f s | c-p >= 0.1 = do return s { gameState = gs {
                                player = ps {
                                    pFrame = if fr == length anim - 1 then 0 else fr+1
                                    },
                                prevClock = c
                                } }
                          | otherwise = do return s
            where
                gs = gameState s
                ps = player gs
                anim = getPlayerAnimation s
                fr = pFrame ps
                c = clock gs
                p = prevClock gs

handleUpdateGameView :: Float -> GlobalState -> IO GlobalState
handleUpdateGameView add s = updatePlayerAnimState add newState
    where
        g = gameState s
        c = clock g
        newState = s { gameState = g { clock = c + add } }