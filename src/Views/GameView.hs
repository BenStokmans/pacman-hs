{-# LANGUAGE BlockArguments #-}
module Views.GameView where
import State
    ( GlobalState(..),
      Settings(..),
      GameState(..), MenuRoute (..) )
import Graphics.Gloss
    ( Picture(Color, Line),
      pictures,
      blank,
      blue,
      green,
      scale,
      translate,
      Color,
      Point, circleSolid, white )
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), MouseButton (..), SpecialKey (..))
import Assets (Assets(..), Anim (..), PacManSprite (..))
import Struct (LevelMap(..), Player(..), Direction (..), Cell(..), CellType(..), Vec2(..), getCell, dirToVec2, oppositeDirection)
import Rendering (renderStringTopLeft)
import FontContainer (FontContainer(..))
import Map (wallSectionToPic, wallToSizedSection, processWalls)
import Data.List (delete)
import Data.Maybe (fromMaybe, isNothing)

gameGridDimensions :: GlobalState -> (Float,Float) --gridsize of level 
gameGridDimensions GlobalState { gameState = GameState { level = (LevelMap w h _) }} = (w,h)

cellSize :: (Float,Float) -> Float -> Float -> (Float,Float) --cellsize in px
cellSize (sx,sy) w h = (w/sx, h/sy)

drawGrid :: (Float, Float) -> Float -> Float -> Color -> Picture
drawGrid (c,r) w h col = Color col $ pictures $ map (\i -> let hc = -w2 + wn*i in Line [(hc, -h2),(hc,h2)]) [0..c] ++ map (\i -> let hr = -h2 + hn*i in Line [(-w2, hr),(w2,hr)]) [0..r]
    where
        w2 = w/2
        h2 = h/2
        (wn,hn) = cellSize (c,r) w h

gridSizePx :: (Float,Float) -> GlobalState -> (Float, Float) --gridsize in pixels onscreen
gridSizePx (c,r) s = (x*0.8*(c/r),y*0.8*(r/c))
    where
        (x,y) = windowSize (settings s)

gridToScreenPos :: GlobalState -> Vec2 -> Point -- get position screen from grid position 
gridToScreenPos s (Vec2 x y) = (x*wn-(w/2)+wn/2, y*hn-(h/2)+hn/2)
    where
        (dim,(w,h)) = gameGridInfo s
        (wn,hn) = cellSize dim w h

screenToGridPos :: GlobalState -> (Float,Float) -> Point -> Vec2 -- get position on grid from screen position
screenToGridPos s (c,r) (x, y) = Vec2 (fromIntegral (floor ((pw/2+x)/cw))) (fromIntegral (floor ((ph/2+y)/ch)))
    where
        (pw,ph) = gridSizePx (c,r) s
        (cw,ch) = cellSize (c,r) pw ph

debugGrid :: GlobalState -> Picture
debugGrid s = let (dim,(w,h)) = gameGridInfo s in drawGrid dim w h green

drawMap :: GlobalState -> LevelMap -> (Float,Float) -> (Float,Float) -> Picture
drawMap s m@(LevelMap _ _ cells) (c,r) (w,h) = Color blue $ pictures $
        map (\(Cell _ (Vec2 x y),ws) -> translate (x*wn-w2+wn/2) (y*hn-h2+hn/2) (wallToSizedSection margin t wn hn ws)) (processWalls m) ++
        map (\(Cell t (Vec2 x y)) -> Color white $ translate (x*wn-w2+wn/2) (y*hn-h2+hn/2) $ scale 1 (hn/wn) $ circleSolid (wn/16)) (filter (\(Cell t _) -> t == Pellet) cells) ++
        map (\(Cell t (Vec2 x y)) -> translate (x*wn-w2+wn/2) (y*hn-h2+hn/2) $ scale ((hn/32)*(1+margin*2)*(c/r)) ((wn/32)*(1+margin*2)*(r/c)) $ appleSprite ass) (filter (\(Cell t _) -> t == PowerUp) cells)
    where
        ass = assets s
        margin = mazeMargin $ settings s
        t = lineThickness $ settings s
        w2 = w/2
        h2 = h/2
        (wn,hn) = cellSize (c,r) w h

getPlayerAnimation :: GlobalState -> Anim
getPlayerAnimation s | d == South = down as
                     | d == West = left as
                     | d == East = right as
                     | otherwise = up as
                where
                    d = pDirection $ player $ gameState s
                    as = pacSprite $ assets s

--                                 c     r     w     h
gameGridInfo :: GlobalState -> ((Float,Float),(Float,Float))
gameGridInfo gs = let (x, y) = gameGridDimensions gs in ((x,y), gridSizePx (x,y) gs)

drawPlayer :: GlobalState -> Picture
drawPlayer s = translate px py $ scale scalarX scalarY (getPlayerAnimation s !! frame)
    where
        (px,py) = pLocation $ player $ gameState s
        frame = pFrame $ player $ gameState s
        ((c,r),(w,h)) = gameGridInfo s
        (wc,hc) = cellSize (c,r) w h
        m = mazeMargin $ settings s
        p = pacmanPadding $ settings s
        pacmanScalar = (1+m*2)*(1-p*2)
        scalarX = (wc/16)*pacmanScalar*(c/r)
        scalarY = (hc/16)*pacmanScalar*(r/c)

renderGameView :: GlobalState -> IO Picture
renderGameView gs = do
    debugString <- renderStringTopLeft (-400,400) (s (emuFont (assets gs))) green
            ("Maze margin: " ++ show (mazeMargin $ settings gs) ++ "\nPacman padding: " ++ show (pacmanPadding $ settings gs))
    return (pictures [grid, drawMap gs m dims gridS, drawPlayer gs, debugString])
    where
        (dims,gridS) = gameGridInfo gs
        m = level $ gameState gs
        grid = if enableDebugGrid $ settings gs then debugGrid gs else blank

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
handleInputGameView (EventKey (SpecialKey KeyEsc) _ _ _) s = do return s {route = PauseMenu, lastRoute = GameView}
handleInputGameView (EventKey (Char 'g') _ _ _) s@(GlobalState { settings = set }) = do return s {settings = set { enableDebugGrid = not (enableDebugGrid set) }}
handleInputGameView (EventKey k _ _ _) s = do return s { gameState = gs { player = ps { pBufferedInput = bufferedInput, pDirection = direction } } }
                        where
                            gs = gameState s
                            ps = player gs
                            newDir = keyToDirection oldDir k
                            oldDir = pDirection ps
                            bufferedInput | newDir == oldDir || newDir == oppositeDirection oldDir = pBufferedInput ps
                                          | otherwise = Just newDir
                            direction | newDir /= oldDir && newDir /= oppositeDirection oldDir = oldDir
                                      | otherwise = newDir -- technically not proper but it works
handleInputGameView _ s = do return s

updatePlayerAnimState :: GlobalState -> IO GlobalState
updatePlayerAnimState s | c-p >= 0.1 = do return s { gameState = gs {
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
                c = clock s
                p = prevClock gs

updatePlayerPosition :: Float -> GlobalState -> GlobalState
updatePlayerPosition dt s | isPastCentre = s { gameState = gs 
                                                {
                                                    player = ps {
                                                        pLocation = if dir /= newDir then pastCentreLocation else newLoc, 
                                                        pDirection = newDir,
                                                        pBufferedInput = if dir /= newDir then Nothing else bufferedInput
                                                        },
                                                    level = LevelMap lw lh newCells
                                                }
                                            }
                          | otherwise = s { gameState = gs {player = ps {pLocation = newLoc }}}
            where
                gs = gameState s
                m@(LevelMap lw lh cells) = level gs
                ps = player gs
                (wc,hc) = cellSize (c,r) w h
                v = pVelocity ps
                ((c,r),(w,h)) = gameGridInfo s
                dir = pDirection ps
                (px,py) = pLocation ps
                distMoved = dt * v
                (Cell ntype _) = fromMaybe (Cell Empty (Vec2 0 0)) (getCell m (gridpos + dirToVec2 dir))
                (x,y)   | dir == North && abs py >= h/2 = (px,-h/2)
                        | dir == South && abs py >= h/2 = (px,h/2)
                        | dir == East &&  abs px >= w/2 = (-w/2, py)
                        | dir == West &&  abs px >= w/2 = (w/2, py)
                        | otherwise = (px,py)
                newLoc@(nx,ny) | ntype == Wall && isPastCentre = (x,y)
                               | dir == North = (x,y+distMoved) 
                               | dir == East = (x + distMoved, y)
                               | dir == South = (x, y - distMoved) 
                               | dir == West = (x - distMoved, y)
                gridpos = screenToGridPos s (c,r) (pLocation ps)
                mbCell = getCell m gridpos 
                (cx, cy) = gridToScreenPos s gridpos
                isPastCentre | dir == North = cy <= y
                             | dir == East = cx <= x 
                             | dir == South = cy > y 
                             | dir == West = cx > x
                cell@(Cell ctype cLoc) = fromMaybe (Cell Empty (Vec2 0 0)) mbCell -- it is assumed that it is not nothing
                bufferedInput = pBufferedInput ps
                canTurn = maybe False (\d -> maybe False (\(Cell t _) -> t /= Wall) $ getCell m (gridpos + dirToVec2 d) ) bufferedInput
                newDir | canTurn = fromMaybe North bufferedInput
                       | otherwise = dir
                pastCentreLocation | newDir == North || newDir == South = (cx, ny)
                                   | otherwise = (nx, cy)
                newCells | ctype == Pellet || ctype == PowerUp = Cell Empty cLoc : delete cell cells 
                            -- TODO: handle other updates needed when pellet or powerup is eaten
                         | otherwise = cells
                

handleUpdateGameView :: Float -> GlobalState -> IO GlobalState
handleUpdateGameView f gs = do 
    ngs <- updatePlayerAnimState gs
    return $ updatePlayerPosition f ngs


