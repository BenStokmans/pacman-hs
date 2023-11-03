{-# LANGUAGE BlockArguments #-}

module Views.GameView where

import Assets (Anim(..), Assets(..), PacManSprite(..))
import Control.Monad.Random
import Data.Foldable (foldrM)
import Data.List (delete, minimumBy)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import FontContainer (FontContainer(..))
import GHC.Base (undefined)
import Graphics.Gloss (Color, Picture(Color, Line), Point, blank, blue, circleSolid, green, makeColor, orange, pictures, red, scale, translate, white)
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), SpecialKey(..))
import Map
  ( calcNextGhostPosition
  , calcNextPlayerPosition
  , calcWrappedPosition
  , deleteMultiple
  , getGhostSpawnPoint
  , isPastCentre
  , processWalls
  , wallSectionToPic
  , wallToSizedSection, getAllowedGhostDirections
  )
import Rendering (cellSize, drawGrid, gridToScreenPos, renderStringTopLeft, renderStringTopRight, screenToGridPos, translateCell)
import State (GameState(..), GlobalState(..), MenuRoute(..), Settings(..), getGhostActor, ghostToSprite, gridSizePx)
import Struct
  ( Cell(..)
  , CellType(..)
  , Direction(..)
  , GhostActor(..)
  , GhostBehaviour(..)
  , GhostType(..)
  , GridInfo
  , LevelMap(..)
  , Player(..)
  , Vec2(..)
  , allDirections
  , cellHasType
  , cellsWithType
  , cellsWithTypeMap
  , clearCell
  , dirToVec2
  , dummyCell
  , getCell
  , getCellCond
  , getCellType
  , ghosts
  , isCellCond
  , isOutOfBounds
  , oppositeDirection
  , outOfBounds
  , scaleVec2
  , setCell
  )
import Pathfinding (getPathLimited, vec2Dist, getAdjacentVecs, getDirectionsLimited)

gameGridDimensions :: GlobalState -> (Float, Float) -- grid size of map
gameGridDimensions GlobalState {gameState = GameState {gMap = (LevelMap w h _)}} = (w, h)

debugGrid :: GlobalState -> Picture
debugGrid s = drawGrid (gameGridInfo s) green

pelletColor :: Color
pelletColor = makeColor 0.96 0.73 0.61 1

getGhostColor :: GhostType -> Color
getGhostColor Blinky = red
getGhostColor Pinky = makeColor 1 0.72 1 1
getGhostColor Inky = makeColor 0 1 1 1
getGhostColor Clyde = makeColor 1 0.72 0.32 1

debugGhostPath :: GlobalState -> Picture
debugGhostPath s =
  pictures $
  map
    (\g ->
       Color (getGhostColor g) $
       pictures $
       maybe
         []
         (map
            (\v ->
               let (x, y) = gridToScreenPos dims v
                in translate x y $ scale 1 (ch / cw) $ circleSolid (cw / 3))) $
       getPathLimited
         (gMap gs)
         (oppositeDirection $ gDirection $ getGhostActor s g)
         (screenToGridPos dims (gLocation $ getGhostActor s g))
         (gTarget $ getGhostActor s g)
         True)
    ghosts
  where
    gs = gameState s
    dims@((c, r), (w, h)) = gameGridInfo s
    (cw, ch) = cellSize dims

debugGhostTargets :: GlobalState -> Picture
debugGhostTargets s =
  pictures $
  map
    (\gt ->
       Color (getGhostColor gt) $
       let (x, y) = gridToScreenPos dims (gTarget $ getGhostActor s gt)
        in translate x y $ scale 1 (ch / cw) $ circleSolid (cw / 3))
    ghosts
  where
    dims@((c, r), (w, h)) = gameGridInfo s
    (cw, ch) = cellSize dims

drawMap :: GlobalState -> LevelMap -> GridInfo -> Picture
drawMap gs m@(LevelMap _ _ cells) gi@((col, row), (w, h)) =
  Color blue $
  pictures $
  map (\(c, ws) -> Color blue $ translateCell c gi (wallToSizedSection margin t cw ch ws)) (filter (cellHasType Wall . fst) $ cachedWalls gs) ++
  map (\(c, ws) -> Color orange $ translateCell c gi (wallToSizedSection margin t cw ch ws)) (filter (cellHasType GhostWall . fst) $ cachedWalls gs) ++
  map (\c -> Color pelletColor $ translateCell c gi $ scale 1 (ch / cw) $ circleSolid (cw / 16)) (cellsWithTypeMap Pellet m) ++
  map (\c -> Color pelletColor $ translateCell c gi $ scale 1 (ch / cw) $ circleSolid (cw / 3)) (cellsWithTypeMap PowerUp m)
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

drawGhost :: GlobalState -> GhostType -> GridInfo -> Point -> Picture
drawGhost gs gt gi@((c, r), _) (px, py) = translate px py $ scale scalarX scalarY (ghostToSprite gs gt)
  where
    (cw, ch) = cellSize gi
    ghostScalar = (1 + mazeMargin (settings gs) * 2) * (1 - ghostPadding (settings gs) * 2)
    scalarX = (cw / 16) * ghostScalar * (c / r)
    scalarY = (ch / 16) * ghostScalar * (r / c)

drawPlayer :: GlobalState -> GridInfo -> Point -> Picture
drawPlayer gs gi@((c, r), _) (px, py) = translate px py $ scale scalarX scalarY (getPlayerAnimation gs !! pFrame (player $ gameState gs))
  where
    (cw, ch) = cellSize gi
    pacmanScalar = (1 + mazeMargin (settings gs) * 2) * (1 - pacmanPadding (settings gs) * 2)
    scalarX = (cw / 16) * pacmanScalar * (c / r)
    scalarY = (ch / 16) * pacmanScalar * (r / c)

renderGameView :: GlobalState -> IO Picture
renderGameView gs = do
  let currentLevel = gMap $ gameState gs
  let gi = gameGridInfo gs
  debugString <-
    renderStringTopRight
      (400, 400)
      (s (emuFont (assets gs)))
      green
      ("Maze margin: " ++
       show (mazeMargin $ settings gs) ++
       "\nPac-Man padding: " ++
       show (pacmanPadding $ settings gs) ++
       "\nBlinky: " ++
       show (screenToGridPos gi $ gLocation $ blinky $ gameState gs) ++
       ", " ++
       show (gTarget $ blinky $ gameState gs) ++
       ", " ++
       show (gDirection $ blinky $ gameState gs) ++
        ", " ++
       show (getAllowedGhostDirections currentLevel (gDirection $ blinky $ gameState gs) (screenToGridPos gi $ gLocation $ blinky $ gameState gs)) ++
       "\nInky: " ++
       show (screenToGridPos gi $ gLocation $ inky $ gameState gs) ++
       ", " ++
       show (gTarget $ inky $ gameState gs) ++
       ", " ++
       show (gDirection $ inky $ gameState gs) ++
       "\nPinky: " ++
       show (screenToGridPos gi $ gLocation $ pinky $ gameState gs) ++
       ", " ++
       show (gTarget $ pinky $ gameState gs) ++
       ", " ++
       show (gDirection $ pinky $ gameState gs) ++
       "\nClyde: " ++
       show (screenToGridPos gi $ gLocation $ clyde $ gameState gs) ++
       ", " ++ show (gTarget $ clyde $ gameState gs) ++ ", " ++ show (gDirection $ clyde $ gameState gs))
  scoreString <- renderStringTopLeft (-400, 400) (FontContainer.m (emuFont (assets gs))) white $ "Score: " ++ show (score $ gameState gs)
  let drawnMap = drawMap gs currentLevel gi
  let drawnGhosts = pictures $ map (\t -> drawGhost gs t gi $ gLocation (getGhostActor gs t)) [Blinky, Pinky, Inky, Clyde]
  let grid =
        if enableDebugGrid $ settings gs
          then debugGrid gs
          else blank
  return
    (pictures
       [ grid
       , drawnMap
       , drawPlayer gs gi (pLocation $ player $ gameState gs)
       , drawnGhosts
       , debugString
       , scoreString
       , debugGhostTargets gs
       , debugGhostPath gs
       ])

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

calculateScatterTarget :: GhostType -> Point -> Vec2 -- after certain amount of dots blinky goes to chase even in scatter
calculateScatterTarget gt (xmax, ymax)
  | gt == Blinky = Vec2 xmax ymax
  | gt == Pinky = Vec2 0 ymax
  | gt == Inky = Vec2 xmax 0
  | gt == Clyde = Vec2 1 1

calculateChaseTarget :: GhostType -> GlobalState -> Vec2
calculateChaseTarget gt s
  | gt == Blinky = pLoc
  | gt == Pinky = pLoc + scaleVec2 pDir 4
  | gt == Inky = blinkyPos + scaleVec2 (pLoc + scaleVec2 pDir 2 - blinkyPos) 2 --check if work; does not work lol!!!!
  | gt == Clyde && sqrt (vec2Dist pLoc clydePos) < 8 = calculateScatterTarget Clyde (gameGridDimensions s)
  | otherwise = pLoc
  where
    p = player gs
    gs = gameState s
    gInfo = gameGridInfo s
    pLoc = screenToGridPos gInfo (pLocation p)
    pDir = dirToVec2 (pDirection p)
    blinkyPos = screenToGridPos gInfo (gLocation $ blinky gs)
    clydePos = screenToGridPos gInfo (gLocation $ clyde gs)

normalizeTarget :: LevelMap -> Vec2 -> Vec2
normalizeTarget m v
  | (_, Just r) <- normalizeTarget' m [v] = r
  | otherwise = error "impressive" -- this can never happen
  where
    normalizeTarget' :: LevelMap -> [Vec2] -> ([Vec2], Maybe Vec2)
    normalizeTarget' _ [] = ([], Nothing)
    normalizeTarget' l current
      | null valid = normalizeTarget' l $ concatMap getAdjacentVecs current
      | otherwise = ([], Just $ minimumBy (\x y -> compare (vec2Dist x v) (vec2Dist y v)) valid)
      where
        valid =
          filter
            (\w ->
               let (Cell ct v) = fromMaybe (Cell Wall (Vec2 0 0)) (getCell l w)
                in ct /= Wall && not (isOutOfBounds l v))
            current

updateGhostTarget :: GhostActor -> GlobalState -> IO GlobalState
-- lvl 1 pinky leaves house instantly, inky after 30 dots clyde after 90
-- lvl 2 pinky and inky leave instantly, clyde after 50 dots
-- lvl 3 everyone leaves instantly
-- for now everyone always leaves instantly
updateGhostTarget ghost s
  | not mustPathfind = do return s
  | ghostState == Frightened && mustPathfind = do
    x <- getRandomR (0 :: Integer, round xmax)
    y <- getRandomR (0 :: Integer, round ymax)
    return $ newState x y
  | otherwise = do return $ newState 0 0
  where
    ghostState = gCurrentBehaviour ghost
    ghostT = ghostType ghost
    m@(LevelMap lw lh cells) = gMap gs
    gi@((xmax, ymax), _) = gameGridInfo s
    gs = gameState s
    currentDirection = gDirection ghost
    (px, py) = gLocation ghost
    currentGridPos = screenToGridPos gi (px, py)
    (cx, cy) = gridToScreenPos gi currentGridPos
    isPastCentre
      | currentDirection == North = cy <= py
      | currentDirection == East = cx <= px
      | currentDirection == South = cy >= py
      | currentDirection == West = cx >= px
    mustPathfind =
      isPastCentre &&
      length
        (cellsWithType
           Wall
           (mapMaybe
              (\d -> getCell m (currentGridPos + dirToVec2 d))
              (deleteMultiple [oppositeDirection currentDirection, currentDirection] allDirections))) <
      2
    t x y
      | ghostState == Scatter = calculateScatterTarget ghostT (xmax, ymax)
      | ghostState == Chase = calculateChaseTarget ghostT s
      | ghostState == Frightened = Vec2 (fromIntegral x :: Float) (fromIntegral y :: Float)
    newState x y
      | ghostT == Blinky = s {gameState = gs {blinky = ghost {gTarget = normalizeTarget m $ t x y, gUpdate = clock s}}}
      | ghostT == Pinky = s {gameState = gs {pinky = ghost {gTarget = normalizeTarget m $ t x y, gUpdate = clock s}}}
      | ghostT == Inky = s {gameState = gs {inky = ghost {gTarget = normalizeTarget m $ t x y, gUpdate = clock s}}}
      | ghostT == Clyde = s {gameState = gs {clyde = ghost {gTarget = normalizeTarget m $ t x y, gUpdate = clock s}}}

updateGhostPosition :: Float -> GlobalState -> GhostActor -> GlobalState
updateGhostPosition dt s ghost = s {gameState = newGameState}
  where
    gs = gameState s
    dims@((c, r), (w, h)) = gameGridInfo s
    (wc, hc) = cellSize dims
    m@(LevelMap lw lh cells) = gMap gs
    currentDirection = gDirection ghost
    location = gLocation ghost
    currentGridPos = screenToGridPos dims location
    distMoved = dt * gVelocity ghost
    nextCellType = getCellType m (currentGridPos + dirToVec2 currentDirection)
    wrappedPos = calcWrappedPosition dims currentDirection location
    pastCenter = isPastCentre dims currentDirection currentGridPos wrappedPos
    newLoc@(nx, ny) = calcNextGhostPosition currentDirection nextCellType pastCenter wrappedPos distMoved
    (cx, cy) = gridToScreenPos dims currentGridPos
    Cell ctype cLoc = fromMaybe dummyCell (getCell m currentGridPos) -- it is assumed that it is not nothing
    walls = cellsWithType
           Wall
           (mapMaybe
              (\d -> getCell m (currentGridPos + dirToVec2 d))
              (deleteMultiple allDirections [oppositeDirection currentDirection, currentDirection]))
    path = fromMaybe [currentDirection] $ getDirectionsLimited m (oppositeDirection currentDirection) currentGridPos (gTarget ghost) True
    allowedDirections = getAllowedGhostDirections m currentDirection currentGridPos

    oldChange = lastDirChange ghost
    (newDir, newChange)
      | oldChange == currentGridPos = (currentDirection, oldChange)
      | gTarget ghost == currentGridPos && not (null allowedDirections) = (head allowedDirections, currentGridPos)
      | gTarget ghost == currentGridPos && null allowedDirections = (oppositeDirection currentDirection, currentGridPos)
      | null path = (currentDirection, oldChange)
      | pastCenter && length walls < 2 = (head path, currentGridPos)
      | otherwise = (currentDirection, oldChange)
    pastCentreLocation
      | newDir == North || newDir == South = (cx, ny)
      | otherwise = (nx, cy)
    finalLocation
      | currentDirection /= newDir = pastCentreLocation
      | otherwise = newLoc
    newGhost = ghost {gLocation = finalLocation, gDirection = newDir, lastDirChange = newChange}
    newGameState
      | ghostType ghost == Blinky = gs {blinky = newGhost}
      | ghostType ghost == Pinky = gs {pinky = newGhost}
      | ghostType ghost == Inky = gs {inky = newGhost}
      | ghostType ghost == Clyde = gs {clyde = newGhost}

updatePlayerPosition :: Float -> GlobalState -> GlobalState
updatePlayerPosition dt s
  | pastCenter =
    s
      { gameState =
          gs
            { score = newScore
            , pelletCount = newPelletCount
            , player =
                ps
                  { pLocation = finalLocation
                  , pDirection = newDir
                  , pBufferedInput =
                      if currentDirection /= newDir
                        then Nothing
                        else bufferedInput
                  , pMoving = finalLocation /= location
                  }
            , gMap = m
            }
      }
  | otherwise = s {gameState = gs {player = ps {pLocation = newLoc, pMoving = newLoc /= location}}}
  where
    gs = gameState s
    dims@((c, r), (w, h)) = gameGridInfo s
    (wc, hc) = cellSize dims
    m@(LevelMap lw lh cells) = gMap gs
    ps = player gs
    currentDirection = pDirection ps
    location = pLocation ps
    currentGridPos = screenToGridPos dims location
    distMoved = dt * pVelocity ps
    nextCellType = getCellType m (currentGridPos + dirToVec2 currentDirection)
    wrappedPos = calcWrappedPosition dims currentDirection location
    pastCenter = isPastCentre dims currentDirection currentGridPos wrappedPos
    newLoc@(nx, ny) = calcNextPlayerPosition currentDirection nextCellType pastCenter wrappedPos distMoved
    (cx, cy) = gridToScreenPos dims currentGridPos
    Cell ctype cLoc = fromMaybe dummyCell (getCell m currentGridPos) -- it is assumed that it is not nothing
    bufferedInput = pBufferedInput ps
    canTurn =
      maybe False (\d -> isCellCond m (\c -> not (cellHasType Wall c) && not (cellHasType GhostWall c)) (currentGridPos + dirToVec2 d)) bufferedInput
    newDir
      | canTurn = fromMaybe North bufferedInput
      | otherwise = currentDirection
    newDirLocation
      | newDir == North || newDir == South = (cx, ny)
      | otherwise = (nx, cy)
    finalLocation
      | currentDirection /= newDir = newDirLocation
      | otherwise = newLoc
    oldScore = score gs
    oldPelletCount = pelletCount gs
    (newScore, newPelletCount, newCells)
      | ctype == Pellet = (oldScore + 1, oldPelletCount + 1, clearCell m cLoc)
      | ctype == PowerUp =
        ( oldScore + 1
        , oldPelletCount
        , clearCell m cLoc -- clean this up
         )
      | otherwise = (oldScore, oldPelletCount, m)

handleUpdateGameView :: Float -> GlobalState -> IO GlobalState
handleUpdateGameView f gs = do
  ngs <- updatePlayerAnimState gs
  let pUpdate = updatePlayerPosition f ngs
  ghostTargetUpdate <- foldrM (\v acc -> updateGhostTarget (getGhostActor acc v) acc) pUpdate ghosts
  return $ foldr (\v acc -> updateGhostPosition f acc (getGhostActor acc v)) ghostTargetUpdate ghosts
