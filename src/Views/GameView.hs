{-# LANGUAGE BlockArguments #-}

module Views.GameView where

import Assets (Anim(..), Assets(..), PacManSprite(..))
import Control.Monad.Random
import Data.Foldable (foldrM)
import Data.List (delete, minimumBy)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import FontContainer (FontContainer(..))
import GHC.Base (undefined)
import Graphics.Gloss (Color, Picture(Color, Line), Point, blank, blue, circleSolid, green, makeColor, orange, pictures, red, scale, translate, white, rectangleWire)
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
  , wallToSizedSection, getAllowedGhostDirections, getSpawnPoint
  )
import Pathfinding ( getDirectionsLimited, getPathLimited )
import Rendering (cellSize, drawGrid, gridToScreenPos, renderStringTopLeft, renderStringTopRight, screenToGridPos, translateCell, resize)
import State (GameState(..), GlobalState(..), MenuRoute(..), Settings(..), getGhostActor, ghostToSprite, gridSizePx, gameGridInfo, ghostActors)
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
import GhostLogic ( updateGhostTarget, updateGhostClock, setGhostBehaviour, updateGhostGlobalState, updateGhostVelocity )

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
  map (\c -> Color pelletColor $ translateCell c gi $ scale 1 (ch / cw) $ circleSolid (cw / 12)) (cellsWithTypeMap Pellet m) ++
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

calcSpriteSize :: GridInfo -> Float -> (Float, Float)
calcSpriteSize gi@((c, r), _) scalar = let (cw, ch) = cellSize gi in (16 * ((cw / 16) * scalar * (c / r)), 16 * ((ch / 16) * scalar * (r / c)))

calcGhostSize :: GlobalState -> GridInfo -> (Float,Float)
calcGhostSize gs gi = calcSpriteSize gi ((1 + mazeMargin (settings gs) * 2) * (1 - ghostPadding (settings gs) * 2))

calcPlayerSize :: GlobalState -> GridInfo -> (Float,Float)
calcPlayerSize gs gi = calcSpriteSize gi ((1 + mazeMargin (settings gs) * 2) * (1 - pacmanPadding (settings gs) * 2))

drawGhost :: GlobalState -> GhostActor -> GridInfo -> Point -> Picture
drawGhost gs ghost gi (px, py) | ghostM == Respawning = translate px py $ scale (timer/respawnLength) (timer/respawnLength) sprite
                               | otherwise = translate px py sprite
    where
      ghostM = gCurrentBehaviour ghost
      (w,h) = calcGhostSize gs gi
      sprite = resize 16 16 w h (ghostToSprite gs ghost)
      timer = gRespawnTimer ghost
      respawnLength = ghostRespawnTimer $ settings gs

drawPlayer :: GlobalState -> GridInfo -> Point -> Picture
drawPlayer gs gi (px, py) = let (w,h) = calcPlayerSize gs gi in translate px py $ resize 16 16 w h (getPlayerAnimation gs !! pFrame (player $ gameState gs))

ghostPlayerCollision :: GlobalState -> GridInfo -> GhostActor -> Bool
ghostPlayerCollision gs gi ga | abs (px - gx) <= (pw/2 + gw/2)*(1-leniency) && abs (py - gy) <= (ph/2 + gh/2)*(1-leniency) = True
                              | otherwise = False
  where
    leniency = collisionLeniency $ settings gs
    (gw,gh) = calcGhostSize gs gi
    (pw,ph) = calcPlayerSize gs gi
    (gx,gy) = gLocation ga
    (px,py) = pLocation $ player $ gameState gs

getGhostDebugString :: GlobalState -> GhostType -> String
getGhostDebugString gs gt = show (screenToGridPos gi $ gLocation ghost) ++
       ", " ++
       show (gTarget ghost) ++
       ", " ++
       show (gDirection ghost) ++
        ", " ++
       show (getAllowedGhostDirections (gMap $ gameState gs) (gDirection ghost) (screenToGridPos gi $ gLocation ghost))  ++
       ", " ++
       show (gVelocity ghost)
       ++ "\n"
       where
        gi = gameGridInfo gs
        ghost = getGhostActor gs gt

drawGhostsBoundingBox :: GlobalState -> Picture
drawGhostsBoundingBox gs = Color white $ pictures $ map (\g -> let (gx,gy) = gLocation g in translate gx gy $ rectangleWire gw gh) $ ghostActors gs
    where (gw,gh) = calcGhostSize gs (gameGridInfo gs)
  
drawPlayerBoundingBox :: GlobalState -> Picture
drawPlayerBoundingBox gs = Color white $ translate px py $ rectangleWire pw ph
    where 
      (px,py) = pLocation $ player $ gameState gs
      (pw,ph) = calcPlayerSize gs (gameGridInfo gs)

getDebugPicture :: GlobalState -> IO Picture
getDebugPicture gs = do
  debugString <- renderStringTopRight
    (400, 400)
    (s (emuFont (assets gs)))
    green
    ("Maze margin: " ++
      show (mazeMargin $ settings gs) ++
      "\nPac-Man padding: " ++
      show (pacmanPadding $ settings gs) ++
      "\nBlinky: " ++ getGhostDebugString gs Blinky ++
      "Inky: " ++ getGhostDebugString gs Inky ++
      "Pinky: " ++ getGhostDebugString gs Pinky ++
      "Clyde: " ++ getGhostDebugString gs Clyde
      )
  let debugs = [
          debugGrid gs,
          debugString,
          drawPlayerBoundingBox gs,
          drawGhostsBoundingBox gs,
          debugGhostTargets gs,
          debugGhostPath gs
          ]
  return $ if debugEnabled $ settings gs then pictures debugs else blank

renderGameView :: GlobalState -> IO Picture
renderGameView gs = do
  let currentLevel = gMap $ gameState gs
  let gi = gameGridInfo gs
  scoreString <- renderStringTopLeft (-400, 400) (FontContainer.m (emuFont (assets gs))) white $ "Score: " ++ show (score $ gameState gs)
  let drawnMap = drawMap gs currentLevel gi
  let drawnGhosts = pictures $ map (\t -> let ghost = getGhostActor gs t in drawGhost gs ghost gi $ gLocation ghost) [Blinky, Pinky, Inky, Clyde]
  let drawnLives = pictures $ map (\v -> translate ((- 375) + 40 * (fromIntegral v :: Float)) (- 375) $ scale 2 2 $ head (right $ pacSprite $ assets gs)) [0..lives (gameState gs)-1]
  debug <- getDebugPicture gs
  return
    (pictures
       [ drawnMap
       , drawnLives
       , drawPlayer gs gi (pLocation $ player $ gameState gs)
       , drawnGhosts
       , scoreString
       , debug
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
      -- | pastCenter && not (null allowedDirections)
      | oldChange == currentGridPos = (currentDirection, oldChange)
      | gTarget ghost == currentGridPos && not (null allowedDirections) = (head allowedDirections, currentGridPos)
      | gTarget ghost == currentGridPos && null allowedDirections = (oppositeDirection currentDirection, currentGridPos) -- this doesn't work exactly like I want it to
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
  | ctype == PowerUp = foldr (\v acc -> setGhostBehaviour acc (getGhostActor acc v) Frightened) newState ghosts
  | otherwise = newState
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
      | ctype == Pellet = (oldScore + 10, oldPelletCount + 1, clearCell m cLoc)
      | ctype == PowerUp =
        ( oldScore + 50
        , oldPelletCount
        , clearCell m cLoc
         )
      | otherwise = (oldScore, oldPelletCount, m)
    newState | pastCenter = s { gameState =
                  gs
                    { score = newScore
                    , pelletCount = newPelletCount
                    , killingSpree = if ctype == PowerUp then 1 else killingSpree gs
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

checkCollisionsForGhost :: GlobalState -> GhostActor -> GlobalState
checkCollisionsForGhost s ghost | colliding && gCurrentBehaviour ghost == Frightened = deadGhostGS { gameState = (gameState deadGhostGS) { score = score gs + (200*(2^ks)), killingSpree = ks+1 } }
                                | colliding && gCurrentBehaviour ghost == Respawning = s
                                | colliding = deadPlayerGS
                                | otherwise = s
                                where
                                  gi = gameGridInfo s
                                  colliding = ghostPlayerCollision s gi ghost
                                  gs = gameState s
                                  level = gMap gs
                                  ks = killingSpree gs
                                  ghostT = ghostType ghost

                                  spawnPoint = getGhostSpawnPoint level ghostT
                                  respawnPos = gridToScreenPos gi $ getGhostSpawnPoint level ghostT
                                  allowedDirections = filter (\d -> isCellCond level (not . cellHasType Wall) (spawnPoint + dirToVec2 d)) allDirections ++ [gDirection ghost] -- the addition of the current direction is purely a failsafe, this could only happen if a map maker decides to put the ghost in a box
                                  respawnGhost = ghost { gLocation = gridToScreenPos gi $ getGhostSpawnPoint level ghostT, gCurrentBehaviour = Respawning, gFrightenedClock = 0, gDirection = head allowedDirections, lastDirChange = spawnPoint }
                                  deadGhostGS = updateGhostGlobalState s respawnGhost

                                  deadPlayerGS | lives gs == 1 = s { route = StartMenu } -- properly handle game over
                                               | otherwise = s { gameState = gs {lives = lives gs - 1, player = (player gs) { pLocation = gridToScreenPos gi $ getSpawnPoint level}}}


handleUpdateGameView :: Float -> GlobalState -> IO GlobalState
handleUpdateGameView f gs = do
  let updatedClocks = foldr (\g acc ->updateGhostClock acc f (getGhostActor gs g)) gs ghosts
  ngs <- updatePlayerAnimState updatedClocks
  let pUpdate = updatePlayerPosition f ngs
  -- update ghosts target
  ghostTargetUpdate <- foldrM (\v acc -> updateGhostTarget (getGhostActor acc v) acc) pUpdate ghosts
  -- update ghosts velocity
  let ghostVelocityUpdate = foldr (\v acc -> updateGhostVelocity acc (getGhostActor acc v)) ghostTargetUpdate ghosts
  -- update ghosts position
  let ghostPositionUpdate = foldr (\v acc -> updateGhostPosition f acc (getGhostActor acc v)) ghostVelocityUpdate ghosts
  -- check for ghosts collision with the player
  let collisionUpdate = foldr (\v acc -> checkCollisionsForGhost acc (getGhostActor acc v)) ghostPositionUpdate ghosts
  return collisionUpdate