{-# LANGUAGE BlockArguments #-}

module Views.GameView where

import Assets (Anim(..), Assets(..), PacManSprite(..))
import Data.List (delete)
import Data.Maybe (fromMaybe, isNothing)
import FontContainer (FontContainer(..))
import Graphics.Gloss (Color, Picture(Color, Line), Point, blank, blue, circleSolid, green, orange, pictures, scale, translate, white, makeColor)
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), SpecialKey(..))
import Map (processWalls, wallSectionToPic, wallToSizedSection)
import Rendering (cellSize, gridToScreenPos, renderStringTopLeft, renderStringTopRight, translateCell)
import State (GameState(..), GlobalState(..), MenuRoute(..), Settings(..))
import Struct
  ( Cell(..)
  , CellType(..)
  , Direction(..)
  , GhostType(..)
  , GridInfo
  , LevelMap(..)
  , Player(..)
  , Vec2(..)
  , cellHasType
  , cellsWithType
  , dirToVec2
  , dummyCell
  , getCell
  , oppositeDirection
  )

gameGridDimensions :: GlobalState -> (Float, Float) -- grid size of level
gameGridDimensions GlobalState {gameState = GameState {level = (LevelMap w h _)}} = (w, h)

drawGrid :: GridInfo -> Color -> Picture
drawGrid gi@((c, r), (w, h)) col =
  Color col $
  pictures $
  [ let hc = -w2 + cw * i
   in Line [(hc, -h2), (hc, h2)]
  | i <- [0 .. c]
  ] ++
  [ let hr = -h2 + ch * i
   in Line [(-w2, hr), (w2, hr)]
  | i <- [0 .. r]
  ]
  where
    w2 = w / 2
    h2 = h / 2
    (cw, ch) = cellSize gi

gridSizePx :: (Float, Float) -> GlobalState -> (Float, Float) -- grid size in pixels onscreen
gridSizePx (c, r) gs =
  let (x, y) = windowSize (settings gs)
   in (x * 0.8 * (c / r), y * 0.8 * (r / c))

screenToGridPos :: GlobalState -> GridInfo -> Point -> Vec2 -- get position on grid from screen position
screenToGridPos gs gi@(_, (pw, ph)) (x, y) = Vec2 (fromIntegral (floor ((pw / 2 + x) / cw))) (fromIntegral (floor ((ph / 2 + y) / ch)))
  where
    (cw, ch) = cellSize gi

debugGrid :: GlobalState -> Picture
debugGrid s = drawGrid (gameGridInfo s) green

pelletColor :: Color
pelletColor = makeColor 0.96 0.73 0.61 1

drawMap :: GlobalState -> LevelMap -> GridInfo -> Picture
drawMap gs m@(LevelMap _ _ cells) gi@((col, row), (w, h)) =
  Color blue $
  pictures $
  map (\(c, ws) -> Color blue $ translateCell c gi (wallToSizedSection margin t cw ch ws)) (filter (cellHasType Wall . fst) $ cachedWalls gs) ++
  map (\(c, ws) -> Color orange $ translateCell c gi (wallToSizedSection margin t cw ch ws)) (filter (cellHasType GhostWall . fst) $ cachedWalls gs) ++
  map (\c -> Color pelletColor $ translateCell c gi $ scale 1 (ch / cw) $ circleSolid (cw / 16)) (cellsWithType Pellet cells) ++
  map (\c -> Color pelletColor $ translateCell c gi $ scale 1 (ch / cw) $ circleSolid (cw / 3)) (cellsWithType PowerUp cells)
  --  map (\c -> translateCell c gi $ scale ((ch/32)*(1+margin*2)*(col/row)) ((cw/32)*(1+margin*2)*(row/col)) $ appleSprite ass) (cellsWithType Apple cells)
  where
    ass = assets gs
    margin = mazeMargin $ settings gs
    t = lineThickness $ settings gs
    (w2, h2) = (w / 2, h / 2)
    (cw, ch) = cellSize gi

getPlayerAnimation :: GlobalState -> Anim
getPlayerAnimation gs
  | d == South = down as
  | d == West = left as
  | d == East = right as
  | otherwise = up as
  where
    d = pDirection $ player $ gameState gs
    as = pacSprite $ assets gs

gameGridInfo :: GlobalState -> GridInfo
gameGridInfo gs =
  let (x, y) = gameGridDimensions gs
   in ((x, y), gridSizePx (x, y) gs)

ghostToSprite :: GlobalState -> GhostType -> Picture
ghostToSprite gs Blinky = blinkySprite $ assets gs
ghostToSprite gs Pinky = pinkySprite $ assets gs
ghostToSprite gs Inky = inkySprite $ assets gs
ghostToSprite gs Clyde = clydeSprite $ assets gs

drawGhost :: GlobalState -> GhostType -> GridInfo -> Point -> Picture
drawGhost gs gt gi@((c, r), (w, h)) (px, py) = translate px py $ scale scalarX scalarY (ghostToSprite gs gt)
  where
    (cw, ch) = cellSize gi
    margin = mazeMargin $ settings gs
    padding = ghostPadding $ settings gs
    ghostScalar = (1 + margin * 2) * (1 - padding * 2)
    scalarX = (cw / 16) * ghostScalar * (c / r)
    scalarY = (ch / 16) * ghostScalar * (r / c)

drawPlayer :: GlobalState -> GridInfo -> Point -> Picture
drawPlayer gs gi@((c, r), (w, h)) (px, py) = translate px py $ scale scalarX scalarY (getPlayerAnimation gs !! frame)
  where
    frame = pFrame $ player $ gameState gs
    (cw, ch) = cellSize gi
    margin = mazeMargin $ settings gs
    padding = pacmanPadding $ settings gs
    pacmanScalar = (1 + margin * 2) * (1 - padding * 2)
    scalarX = (cw / 16) * pacmanScalar * (c / r)
    scalarY = (ch / 16) * pacmanScalar * (r / c)

renderGameView :: GlobalState -> IO Picture
renderGameView gs = do
  debugString <-
    renderStringTopRight
      (400, 400)
      (s (emuFont (assets gs)))
      green
      ("Maze margin: " ++ show (mazeMargin $ settings gs) ++ "\nPac-Man padding: " ++ show (pacmanPadding $ settings gs))
  scoreString <- renderStringTopLeft (-400, 400) (FontContainer.m (emuFont (assets gs))) white $ "Score: " ++ show (score $ gameState gs)
  let gi = gameGridInfo gs
  let currentLevel = level $ gameState gs
  let drawnMap = drawMap gs currentLevel gi
  let grid =
        if enableDebugGrid $ settings gs
          then debugGrid gs
          else blank
  return (pictures [grid, drawnMap, drawPlayer gs gi (pLocation $ player $ gameState gs), debugString, scoreString])

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
handleInputGameView (EventKey (SpecialKey KeyEsc) _ _ _) gs = do
  return gs {route = PauseMenu, lastRoute = GameView}
handleInputGameView (EventKey (Char 'g') _ _ _) gs@(GlobalState {settings = set}) = do
  return gs {settings = set {enableDebugGrid = not (enableDebugGrid set)}}
handleInputGameView (EventKey k _ _ _) s = do
  return s {gameState = gs {player = ps {pBufferedInput = bufferedInput, pDirection = direction}}}
  where
    gs = gameState s
    ps = player gs
    newDir = keyToDirection oldDir k
    oldDir = pDirection ps
    bufferedInput
      | newDir == oldDir || newDir == oppositeDirection oldDir = pBufferedInput ps
      | otherwise = Just newDir
    direction
      | newDir /= oldDir && newDir /= oppositeDirection oldDir = oldDir
      | otherwise = newDir -- technically not proper but it works
handleInputGameView _ s = do
  return s

updatePlayerAnimState :: GlobalState -> IO GlobalState
updatePlayerAnimState s
  | not $ pMoving ps = do return s {gameState = gs {prevClock = p + (c - p)}}
  | c - p >= 0.1 = do
    return
      s
        { gameState =
            gs
              { player =
                  ps
                    { pFrame =
                        if fr == length anim - 1
                          then 0
                          else fr + 1
                    }
              , prevClock = c
              }
        }
  | otherwise = do return s
  where
    gs = gameState s
    ps = player gs
    anim = getPlayerAnimation s
    fr = pFrame ps
    c = clock s
    p = prevClock gs

updatePlayerPosition :: Float -> GlobalState -> GlobalState
updatePlayerPosition dt s
  | isPastCentre =
    s
      { gameState =
          gs
            { score = newScore
            , player =
                ps
                  { pLocation = finalLocation
                  , pDirection = newDir
                  , pBufferedInput =
                      if currentDirection /= newDir
                        then Nothing
                        else bufferedInput
                  , pMoving = finalLocation /= (px, py)
                  }
            , level = LevelMap lw lh newCells
            }
      }
  | otherwise = s {gameState = gs {player = ps {pLocation = newLoc, pMoving = newLoc /= (px, py)}}}
  where
    gs = gameState s
    dims@((c, r), (w, h)) = gameGridInfo s
    (wc, hc) = cellSize dims
    m@(LevelMap lw lh cells) = level gs
    ps = player gs
    v = pVelocity ps
    currentDirection = pDirection ps
    (px, py) = pLocation ps
    currentGridPos = screenToGridPos s dims (px, py)
    distMoved = dt * v
    (Cell nextCellType _) = fromMaybe dummyCell (getCell m (currentGridPos + dirToVec2 currentDirection))
    (x, y)
      | currentDirection == North && py >= h / 2 = (px, -h / 2 + hc / 2)
      | currentDirection == South && py <= -h / 2 = (px, h / 2 - hc / 2)
      | currentDirection == East && px >= w / 2 = (-w / 2 + wc / 2, py)
      | currentDirection == West && px <= -w / 2 = (w / 2 - wc / 2, py)
      | otherwise = (px, py)
    newLoc@(nx, ny)
      | nextCellType == Wall && isPastCentre = (x, y)
      | currentDirection == North = (x, y + distMoved)
      | currentDirection == East = (x + distMoved, y)
      | currentDirection == South = (x, y - distMoved)
      | currentDirection == West = (x - distMoved, y)
    (cx, cy) = gridToScreenPos dims currentGridPos
    isPastCentre
      | currentDirection == North = cy <= y
      | currentDirection == East = cx <= x
      | currentDirection == South = cy >= y
      | currentDirection == West = cx >= x
    cell@(Cell ctype cLoc) = fromMaybe dummyCell (getCell m currentGridPos) -- it is assumed that it is not nothing
    bufferedInput = pBufferedInput ps
    canTurn =
      maybe
        False
        (\d -> maybe False (\c -> not (cellHasType Wall c) && not (cellHasType GhostWall c)) $ getCell m (currentGridPos + dirToVec2 d))
        bufferedInput
    newDir
      | canTurn = fromMaybe North bufferedInput
      | otherwise = currentDirection
    pastCentreLocation
      | newDir == North || newDir == South = (cx, ny)
      | otherwise = (nx, cy)
    oldScore = score gs
    (newScore, newCells)
      | ctype == Pellet || ctype == PowerUp = (oldScore + 1, Cell Empty cLoc : delete cell cells)
      | otherwise = (oldScore, cells)
    finalLocation
      | currentDirection /= newDir = pastCentreLocation
      | otherwise = newLoc

handleUpdateGameView :: Float -> GlobalState -> IO GlobalState
handleUpdateGameView f gs = do
  ngs <- updatePlayerAnimState gs
  return $ updatePlayerPosition f ngs
