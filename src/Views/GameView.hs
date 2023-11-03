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
import Pathfinding
import Rendering (cellSize, drawGrid, gridToScreenPos, renderStringTopLeft, renderStringTopRight, screenToGridPos, translateCell)
import State (GameState(..), GlobalState(..), MenuRoute(..), Settings(..), getGhostActor, ghostToSprite, gridSizePx, gameGridInfo)
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
import GhostLogic ( updateGhostTarget )

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
            , gMap = newMap
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
    (newScore, newPelletCount, newMap)
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
