module GameLogic.GhostLogic where
  
import GameLogic.MapLogic
import State
    ( GlobalState(gameState, clock, settings, gameLevel),
      GameState(pauseGameTimer, blinky, pinky, inky, clyde, level,
                pelletCount, gMap, lives, player, godMode, score, killingSpree),
      Settings(ghostStuckTimeout, ghostBlinkLength, ghostRespawnTimer),
      gameGridInfo,
      ghostActors,
      getGhostActor, GhostActor (..), GhostBehaviour (..), Player (..) )
import GameLogic.Pathfinding
    ( getAdjacentVecs,
      getDirectionsLimited,
      getTraveledDirection,
      vec2Dist )
import Data.Ord (clamp)
import Data.List ( minimumBy, delete )
import Data.Maybe ( fromMaybe, mapMaybe )
import Control.Monad.Random ( MonadRandom(getRandomR) )
import GameLogic.GameLogic
    ( calculateGameSpeed, ghostPlayerCollision )
import GameLogic.Struct
import Rendering

-- After certain amount of dots blinky goes to chase even in scatter, this is refered to as "cruise elroy mode" by pacman guru's.
-- Otherwise the scatter target for each ghost is one of the corners.
calculateScatterTarget :: GhostType -> GlobalState -> Vec2
calculateScatterTarget gt s
  | gt == Blinky && hasElroyBoost (level gs) (pelletCount gs) = Vec2 xmax ymax
  | gt == Blinky = Vec2 xmax ymax
  | gt == Pinky = Vec2 0 ymax
  | gt == Inky = Vec2 xmax 0
  | gt == Clyde = Vec2 1 1
    where
      ((xmax, ymax), _) = gameGridInfo s
      gs = gameState s

-- Each ghost has different targeting for more exact details reference: 
-- https://gameinternals.com/understanding-pac-man-ghost-behavior
calculateChaseTarget :: GhostType -> GlobalState -> Vec2
calculateChaseTarget gt s
  | gt == Blinky = pLoc
  | gt == Pinky = pLoc + scaleVec2 pDir 4
  | gt == Inky = blinkyPos + scaleVec2 (pLoc + scaleVec2 pDir 2 - blinkyPos) 2
  | gt == Clyde && vec2Dist pLoc clydePos < 64 = calculateScatterTarget Clyde s
  | otherwise = pLoc
  where
    p = player gs
    gs = gameState s
    gInfo = gameGridInfo s
    pLoc = screenToGridPos gInfo (pLocation p)
    pDir = dirToVec2 (pDirection p)
    blinkyPos = screenToGridPos gInfo (gLocation $ blinky gs)
    clydePos = screenToGridPos gInfo (gLocation $ clyde gs)

-- Normalizes targets out of bounds to the closed position on the grid, so that pathfinding does not take for ever. 
normalizeTarget :: LevelMap -> Vec2 -> Vec2
normalizeTarget m@(LevelMap w h _) (Vec2 x y)
  | (_, Just r) <- normalizeTarget' m [v] = r
  | otherwise = error "impressive" -- this can never happen
  where
    v = Vec2 (clamp (0,w-1) x) (clamp (0,h-1) y)
    normalizeTarget' :: LevelMap -> [Vec2] -> ([Vec2], Maybe Vec2)
    normalizeTarget' _ [] = ([], Nothing)
    normalizeTarget' l current
      | null valid = normalizeTarget' l $ concatMap getAdjacentVecs current
      | otherwise = ([], Just $ minimumBy (\vx vy -> compare (vec2Dist vx v) (vec2Dist vy v)) valid)
      where
        valid =
          filter
            (\z ->
               let (Cell ct cv) = fromMaybe (Cell Wall (Vec2 0 0)) (getCell l z)
                in ct /= Wall && not (isOutOfBounds l cv))
            current

getRandomElement :: [a] -> IO a
getRandomElement [] = error "getRandomElement: empty list"
getRandomElement xs = do
   i <- getRandomR (0, length xs-1)
   return $ xs !! i

updateGhostTarget :: GhostActor -> GlobalState -> IO GlobalState
updateGhostTarget ghost s
  | not mustPathfind || gVelocity ghost == 0 = do return s
  | ghostState == Frightened && mustPathfind = do
    let allowedDirections = filter (\d -> isCellCond m (not . cellHasType Wall) (currentGridPos + dirToVec2 d)) (delete (oppositeDirection currentDirection) allDirections)
    rDir <- if null allowedDirections then do return currentDirection else getRandomElement allowedDirections
    return $ newState $ currentGridPos + scaleVec2 (dirToVec2 rDir) 2 -- the arbitrary scaling here is hacky but it works
  | otherwise = do return $ newState $ currentGridPos + scaleVec2 (dirToVec2 currentDirection) 2
  where
    ghostState = gCurrentBehaviour ghost
    ghostT = ghostType ghost
    m@(LevelMap lw lh cells) = gMap gs
    gi = gameGridInfo s
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
      2 -- Checks if a choice needs to be made otherwise does not need to pathfind. 
    t v
      | ghostState == Scatter = calculateScatterTarget ghostT s
      | ghostState == Chase = calculateChaseTarget ghostT s
      | ghostState == Frightened = v
      | otherwise = currentGridPos
    newState v = updateGhostGlobalState s $ ghost {gTarget = normalizeTarget m $ t v, gUpdate = clock s}

updateGhostGameState :: GameState -> GhostActor -> GameState
updateGhostGameState gs ghost | ghostT == Blinky = gs {blinky = ghost}
                              | ghostT == Pinky = gs {pinky = ghost}
                              | ghostT == Inky = gs {inky = ghost}
                              | ghostT == Clyde = gs {clyde = ghost}
                              where ghostT = ghostType ghost

updateGhostGlobalState :: GlobalState -> GhostActor -> GlobalState
updateGhostGlobalState gs ghost = gs { gameState = updateGhostGameState (gameState gs) ghost }

-- Behaviours of ghost alternate based on the time passed and the level. 
getBehaviour :: Float -> Int -> GhostBehaviour
getBehaviour clock level | level == 1 && clock < 7 = Scatter
                         | level == 1 && clock < 27 = Chase
                         | level == 1 && clock < 34 = Scatter
                         | level == 1 && clock < 54 = Chase
                         | level == 1 && clock < 59 = Scatter
                         | level == 1 && clock < 79 = Chase
                         | level == 1 && clock < 84 = Scatter
                         | level == 1 && clock >= 84 = Chase
                         | level > 1 && level <= 4 && clock < 7 = Scatter
                         | level > 1 && level <= 4 && clock < 27 = Chase
                         | level > 1 && level <= 4 && clock < 34 = Scatter
                         | level > 1 && level <= 4 && clock < 54 = Chase
                         | level > 1 && level <= 4 && clock < 59 = Scatter
                         | level > 1 && level <= 4 && clock < 1092 = Chase
                         | level > 1 && level <= 4 && clock <= 1092 + 1/60 = Scatter
                         | level > 1 && level <= 4 && clock > 1092 + 1/60 = Chase
                         | level > 4 && clock < 5 = Scatter
                         | level > 4 && clock < 25 = Chase
                         | level > 4 && clock < 30 = Scatter
                         | level > 4 && clock < 50 = Chase
                         | level > 4 && clock < 55 = Scatter
                         | level > 4 && clock < 1092 = Chase
                         | level > 4 && clock <= 1092 + 1/60 = Scatter
                         | otherwise = Chase

levelToFrightenDuration :: Int -> Float
levelToFrightenDuration level | level == 1 = 6
                              | level == 2 = 5
                              | level == 3 = 4
                              | level == 4 = 3
                              | level == 5 = 2
                              | level == 6 = 5
                              | level == 7 = 2
                              | level == 8 = 2
                              | level == 9 = 1
                              | level == 10 = 5
                              | level == 11 = 2
                              | level == 12 = 1
                              | level == 13 = 1
                              | level == 14 = 3
                              | level == 15 = 1
                              | level == 16 = 1
                              | level == 18 = 1
                              | otherwise = 0

-- Blink count refers to the amount of times a ghost flashes before leaving frightened mode. 
levelToBlinkCount :: Int -> Float
levelToBlinkCount level | level <= 8 = 5
                        | level == 9 = 3
                        | level == 10 || level == 11 = 5
                        | level == 12 || level == 13 = 3
                        | level == 14 = 5
                        | level == 15 || level == 16 = 3
                        | level == 18 = 3
                        | otherwise = 0

stillFrightened :: Float -> Float -> Int -> Bool
stillFrightened frightenedClock blinkDuration level = frightenedClock < levelToFrightenDuration level + (blinkDuration * levelToBlinkCount level)


-- Updates all ghost timers and their resulting effects. 
updateGhost :: GlobalState -> Float -> GhostActor -> Int -> GhostActor
updateGhost gs dt ghost l | gUpdate ghost > ghostStuckTimeout (settings gs) = updatedGhost {gUpdate = 0, lastDirChange = outOfBounds}
                          | ghostM == Respawning && (gRespawnTimer ghost + dt) < respawnLength = updatedGhost {gRespawnTimer = gRespawnTimer ghost + dt}
                          | ghostM == Respawning && (gRespawnTimer ghost - dt) >= respawnLength = updatedGhost {gRespawnTimer = 0, gCurrentBehaviour = newMode}
                          | ghostM == Frightened && stayFrightened = updatedGhost {gFrightenedClock = gFrightenedClock ghost + dt, gBlink = blink, gAnimClock = newAnimClock}
                          | otherwise = updatedGhost {gCurrentBehaviour = newMode, gFrightenedClock = 0}
  where
    ghostM = gCurrentBehaviour ghost
    newClock = gModeClock ghost + dt
    blinkLength = ghostBlinkLength $ settings gs
    stayFrightened = stillFrightened (gFrightenedClock ghost) blinkLength l
    updatedGhost = ghost {gModeClock = newClock, gUpdate = gUpdate ghost + dt}
    newMode = getBehaviour newClock l
    respawnLength = ghostRespawnTimer $ settings gs
    animClock = gAnimClock ghost
    progressedAnimClock | animClock > 0 = animClock - dt
                        | otherwise = 0
    (blink,newAnimClock) | not stayFrightened = (gBlink ghost,progressedAnimClock)
                         | gFrightenedClock ghost - levelToFrightenDuration l <= 0 = (gBlink ghost, progressedAnimClock)
                         | progressedAnimClock <= 0 = (not $ gBlink ghost,blinkLength/2)
                         | otherwise = (gBlink ghost,progressedAnimClock)

setGhostBehaviour :: GlobalState -> GhostActor -> GhostBehaviour -> GlobalState
setGhostBehaviour s ghost b = updateGhostGlobalState s ghost { gCurrentBehaviour = b, gDirection = direction, lastDirChange = newDirChange, gBlink = False, gAnimClock = 0 }
  where
    ghostT = ghostType ghost
    direction | b == Frightened = oppositeDirection $ gDirection ghost
              | otherwise = gDirection ghost
    newDirChange | b == Frightened = screenToGridPos (gameGridInfo s) (gLocation ghost)
                 | otherwise = lastDirChange ghost

updateGhostClock :: GlobalState -> Float -> GhostActor -> GlobalState
updateGhostClock s dt ghost = updateGhostGlobalState s $ updateGhost s dt ghost l
  where ghostT = ghostType ghost
        l = level $ gameState s

getFrightSpeed :: Int -> Float
getFrightSpeed level | level == 1 = 0.5
                     | level < 5 = 0.55
                     | otherwise = 0.6

getChaseSpeed :: Int -> Float
getChaseSpeed level | level == 1 = 0.75
                    | level < 5 = 0.85
                    | otherwise = 0.95

getWarpSpeed :: Int -> Float
getWarpSpeed level | level == 1 = 0.4
                   | level < 5 = 0.45
                   | otherwise = 0.5


-- The amount of pallets required for the first elroy mode.
getElroyOnePallets :: Int -> Int
getElroyOnePallets l | l == 1 = 20
                     | l == 2 = 30
                     | l < 6 = 40
                     | l < 9 = 50
                     | l < 12 = 60
                     | l < 15 = 80
                     | l < 19 = 100
                     | otherwise = 120

-- The amount of pallets required for the second elroy mode.
getElroyTwoPallets :: Int -> Int
getElroyTwoPallets l = getElroyOnePallets l `div` 2

hasElroyBoost :: Int -> Int -> Bool
hasElroyBoost l p = p < getElroyOnePallets l

hasElroyTwoBoost :: Int -> Int -> Bool
hasElroyTwoBoost l p = p < getElroyTwoPallets l


-- The speedboost the elroy cruise mode gives. 
elroyBoost :: Int -> Int -> Float
elroyBoost l p | hasElroyTwoBoost l p = 0.1
               | hasElroyBoost l p = 0.05
               | otherwise = 0

castRay :: LevelMap -> Vec2 -> (Vec2 -> Bool) -> Direction -> Vec2
castRay m v f d | isOutOfBounds m v = outOfBounds
                | f v = v
                | otherwise = castRay m (v + dirToVec2 d) f d

isIntersection :: LevelMap -> Vec2 -> Bool -- TODO: this doesn't want to work
isIntersection m v = not $ isCellType m Wall v && length (maybe [] (getAdjacent (getCellWithType Wall) m) (getCell m v)) > 2

-- Casts a ray infront and behind pacman if out of bounds anywhere before colliding with a wall or an intersection
-- it implies pacman is in a warp tunnel. 
inWarpTunnel :: GlobalState -> GhostActor -> Bool
inWarpTunnel gs ga = castRay m loc (isCellType m Wall) dir == outOfBounds || castRay m loc (isCellType m Wall) (oppositeDirection dir) == outOfBounds
  where
    m = gameLevel gs
    loc = screenToGridPos (gameGridInfo gs) (gLocation ga)
    dir = gDirection ga

-- Some ghosts remain frozen in spawn before a certain amount of pellets are collected. 
-- Velocity scales with level and different behaviours. 
-- For exact detail reference the following table: 
-- https://pacman.holenet.info/#LvlSpecs
getGhostVelocity :: GlobalState -> GhostActor -> Float
getGhostVelocity s ghost | behaviour == Respawning = 0
                         | l == 1 && ghostT == Inky && pellets < 30 = 0
                         | l == 1 && ghostT == Clyde && pellets < 90 = 0
                         | l == 2 && ghostT == Clyde && pellets < 50 = 0
                         | inWarpTunnel s ghost = getWarpSpeed l
                         | behaviour == Frightened = getFrightSpeed l
                         | ghostT == Blinky = getChaseSpeed l + elroyBoost pellets l
                         | otherwise = getChaseSpeed l
  where
    ghostT = ghostType ghost
    gs = gameState s
    l = level gs
    pellets = pelletCount gs
    behaviour = gCurrentBehaviour ghost

updateGhostVelocity :: GlobalState -> GhostActor -> GlobalState
updateGhostVelocity s ghost = let nv = getGhostVelocity s ghost in updateGhostGlobalState s ghost {
                  gVelocity = nv,
                  lastDirChange = if gVelocity ghost == 0 && nv /= gVelocity ghost then outOfBounds else lastDirChange ghost -- make sure ghosts don't get stuck in spawn
                  }

hasFrightenedGhost :: GlobalState -> Bool
hasFrightenedGhost s = any (\g -> gCurrentBehaviour g == Frightened) (ghostActors s)

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
    distMoved = calculateGameSpeed s currentDirection (gVelocity ghost) * dt
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
      | gTarget ghost == currentGridPos && null allowedDirections && isOutOfBounds m (currentGridPos + dirToVec2 currentDirection) = (currentDirection, oldChange)
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
    newGhost = ghost {gLocation = finalLocation, gDirection = newDir, lastDirChange = newChange, gUpdate = if finalLocation /= location then 0 else gUpdate ghost}
    newGameState = updateGhostGameState gs newGhost

-- Checks if a ghost collides with player and if so updates the gamestate accordingly.
checkCollisionsForGhost :: GlobalState -> GhostActor -> GlobalState
checkCollisionsForGhost s ghost | godMode gs = s
                                | colliding && gCurrentBehaviour ghost == Frightened = deadGhostGS { gameState = (gameState deadGhostGS) { score = score gs + 200*(2^(ks-1)), killingSpree = ks+1, pauseGameTimer = 1 } }
                                | colliding && gCurrentBehaviour ghost == Respawning = s
                                | colliding = foldr (\g ts -> updateGhostGlobalState ts (getGhostActor ts g) {gLocation = gridToScreenPos gi $ getGhostSpawnPoint level g}) deadPlayerGS ghosts
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
                                  deadPlayerGS | lives gs == 1 = s { gameState = gs { lives = 0, pauseGameTimer = 2, player = (player gs) { pLocation = (-1000,-1000)}} }
                                               | otherwise = s { gameState = gs {lives = lives gs - 1, killingSpree = 0, pauseGameTimer = 1, player = (player gs) { pLocation = gridToScreenPos gi $ getSpawnPoint level, pDirection = fromMaybe North $ headMaybe $ map (getTraveledDirection (getSpawnPoint level)) $ filterLevelVec2s level (not . cellHasTypes [Wall,GhostWall]) $ adjacentVecs (getSpawnPoint level)}}}
