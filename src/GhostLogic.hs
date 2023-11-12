module GhostLogic where
import Struct
    ( allDirections,
      cellsWithType,
      dirToVec2,
      getCell,
      isOutOfBounds,
      oppositeDirection,
      scaleVec2,
      Cell(Cell),
      CellType(Wall),
      Direction(West, North, East, South),
      GhostActor(..),
      GhostBehaviour(..),
      GhostType(..),
      LevelMap(..),
      Player(pDirection, pLocation),
      Vec2(..), cellHasType, isCellCond, outOfBounds )
import Graphics.Gloss (Point)
import State (GlobalState (..), GameState (..), gameGridDimensions, gameGridInfo, Settings (..), ghostActors)
import Map (deleteMultiple, getAllowedGhostDirections)
import Pathfinding ( getAdjacentVecs, vec2Dist )
import Rendering ( gridToScreenPos, screenToGridPos )
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.List (minimumBy, delete)
import Control.Monad.Random (getRandomR)
import Data.Tree (levels)
import Data.IntMap (update)
import Data.Ord (clamp)
import Data.Data (ConstrRep(FloatConstr))


calculateScatterTarget :: GhostType -> GlobalState -> Vec2 -- after certain amount of dots blinky goes to chase even in scatter
calculateScatterTarget gt s
  | gt == Blinky && hasElroyBoost (level gs) (pelletCount gs) = Vec2 xmax ymax
  | gt == Blinky = Vec2 xmax ymax
  | gt == Pinky = Vec2 0 ymax
  | gt == Inky = Vec2 xmax 0
  | gt == Clyde = Vec2 1 1
    where 
      ((xmax, ymax), _) = gameGridInfo s
      gs = gameState s
      

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
    rDir <- getRandomElement allowedDirections
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
      2
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

updateGhost :: GlobalState -> Float -> GhostActor -> Int -> GhostActor --TODO: on levels where frightened time is 0 ghosts should still reverse direction 
updateGhost gs dt ghost l | gUpdate ghost > ghostStuckTimeout (settings gs) = updatedGhost {gUpdate = 0, lastDirChange = outOfBounds}
                          | ghostM == Respawning && (gRespawnTimer ghost + dt) < respawnLength = updatedGhost {gRespawnTimer = gRespawnTimer ghost + dt}
                          | ghostM == Respawning && (gRespawnTimer ghost - dt) >= respawnLength = updatedGhost {gRespawnTimer = 0, gCurrentBehaviour = newMode}
                          | ghostM == Frightened && stayFrightened = updatedGhost {gFrightenedClock = gFrightenedClock ghost + dt}
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
    (blink,newAnimClock) | not stayFrightened = (False,0)
                         | gFrightenedClock ghost - levelToFrightenDuration l <= 0 = (False, 0)
                         | newAnimClock <= 0 = (not $ gBlink ghost,blinkLength)
                         | otherwise = (False,0)

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


getElroyOnePallets :: Int -> Int
getElroyOnePallets l | l == 1 = 20
                     | l == 2 = 30
                     | l < 6 = 40
                     | l < 9 = 50
                     | l < 12 = 60
                     | l < 15 = 80
                     | l < 19 = 100
                     | otherwise = 120

getElroyTwoPallets :: Int -> Int
getElroyTwoPallets l = getElroyOnePallets l `div` 2

hasElroyBoost :: Int -> Int -> Bool 
hasElroyBoost l p = p < getElroyOnePallets l 

hasElroyTwoBoost :: Int -> Int -> Bool 
hasElroyTwoBoost l p = p < getElroyTwoPallets l

elroyBoost :: Int -> Int -> Float
elroyBoost l p | hasElroyTwoBoost l p = 0.1
               | hasElroyBoost l p = 0.05
               | otherwise = 0

getGhostVelocity ::  GlobalState -> GhostActor -> Float
-- lvl 1 pinky leaves house instantly, inky after 30 dots clyde after 90
-- lvl 2 pinky and inky leave instantly, clyde after 50 dots
-- lvl 3 everyone leaves instantly
getGhostVelocity s ghost | behaviour == Respawning = 0
                         | l == 1 && ghostT == Inky && pellets < 30 = 0
                         | l == 1 && ghostT == Clyde && pellets < 90 = 0
                         | l == 2 && ghostT == Clyde && pellets < 50 = 0
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