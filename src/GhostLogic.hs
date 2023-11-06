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
      Vec2(..) )
import Graphics.Gloss (Point)
import State (GlobalState (..), GameState (..), gameGridDimensions, gameGridInfo, Settings (..))
import Map (deleteMultiple, getAllowedGhostDirections)
import Pathfinding ( getAdjacentVecs, vec2Dist )
import Rendering ( gridToScreenPos, screenToGridPos )
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.List (minimumBy)
import Control.Monad.Random (getRandomR)
import Data.Tree (levels)
import Data.IntMap (update)
import Data.Ord (clamp)


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
  | gt == Inky = blinkyPos + scaleVec2 (pLoc + scaleVec2 pDir 2 - blinkyPos) 2 
  | gt == Clyde && vec2Dist pLoc clydePos < 64 = calculateScatterTarget Clyde (gameGridDimensions s)
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
getRandomElement xs = do
   i <- getRandomR (0, length xs-1)
   return $ xs !! i

updateGhostTarget :: GhostActor -> GlobalState -> IO GlobalState
updateGhostTarget ghost s
  | not mustPathfind || gVelocity ghost == 0 = do return s
  | ghostState == Frightened && mustPathfind = do
    let allowedDirections = currentDirection : getAllowedGhostDirections m currentDirection currentGridPos
    rDir <- getRandomElement allowedDirections
    return $ newState $ currentGridPos + scaleVec2 (dirToVec2 rDir) 2
  | otherwise = do return $ newState $ currentGridPos + scaleVec2 (dirToVec2 currentDirection) 2
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
    t v
      | ghostState == Scatter = calculateScatterTarget ghostT (xmax, ymax)
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

stillFrightened :: Float -> Int -> Bool
stillFrightened frightenedClock level | level == 1 && frightenedClock < 6 = True
                                      | level == 2 && frightenedClock < 5 = True
                                      | level == 3 && frightenedClock < 4 = True
                                      | level == 4 && frightenedClock < 3 = True
                                      | level == 5 && frightenedClock < 2 = True
                                      | level == 6 && frightenedClock < 5 = True
                                      | level == 7 && frightenedClock < 2 = True
                                      | level == 8 && frightenedClock < 2 = True
                                      | level == 9 && frightenedClock < 1 = True
                                      | level == 10 && frightenedClock < 5 = True
                                      | level == 11 && frightenedClock < 2 = True
                                      | level == 12 && frightenedClock < 1 = True
                                      | level == 13 && frightenedClock < 1 = True
                                      | level == 14 && frightenedClock < 3 = True
                                      | level == 15 && frightenedClock < 1 = True
                                      | level == 16 && frightenedClock < 1 = True
                                      | level == 18 && frightenedClock < 1 = True
                                      | otherwise = False
                                     


updateGhost :: GlobalState -> Float -> GhostActor -> Int -> GhostActor --TODO: on levels where frightened time is 0 ghosts should still reverse direction 
updateGhost gs dt ghost l | ghostM == Respawning && (gRespawnTimer ghost + dt) < respawnLength = updatedGhost {gRespawnTimer = gRespawnTimer ghost + dt}
                          | ghostM == Respawning && (gRespawnTimer ghost - dt) >= respawnLength = updatedGhost {gRespawnTimer = 0, gCurrentBehaviour = newMode}
                          | ghostM == Frightened && stayFrightened = updatedGhost {gFrightenedClock = gFrightenedClock ghost + dt}
                          | otherwise = updatedGhost {gCurrentBehaviour = newMode, gFrightenedClock = 0}
  where 
    ghostM = gCurrentBehaviour ghost
    newClock = gModeClock ghost + dt
    stayFrightened = stillFrightened (gFrightenedClock ghost) l
    updatedGhost = ghost {gModeClock = newClock}
    newMode = getBehaviour newClock l
    respawnLength = ghostRespawnTimer $ settings gs

setGhostBehaviour :: GlobalState -> GhostActor -> GhostBehaviour -> GlobalState
setGhostBehaviour s ghost b = updateGhostGlobalState s ghost { gCurrentBehaviour = b, gDirection = direction, lastDirChange = newDirChange }
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

getGhostVelocity ::  GlobalState -> GhostActor -> Float
-- lvl 1 pinky leaves house instantly, inky after 30 dots clyde after 90
-- lvl 2 pinky and inky leave instantly, clyde after 50 dots
-- lvl 3 everyone leaves instantly
getGhostVelocity s ghost | behaviour == Respawning = 0
                         | l == 1 && ghostT == Inky && pellets < 30 = 0
                         | l == 1 && ghostT == Clyde && pellets < 90 = 0
                         | l == 2 && ghostT == Clyde && pellets < 50 = 0
                         | behaviour == Frightened = 60 -- FIXME: correct speed
                         | otherwise = 75
  where
    ghostT = ghostType ghost
    gs = gameState s
    l = level gs
    pellets = pelletCount gs
    behaviour = gCurrentBehaviour ghost

updateGhostVelocity :: GlobalState -> GhostActor -> GlobalState
updateGhostVelocity s ghost = let nv = getGhostVelocity s ghost in updateGhostGlobalState s ghost {gVelocity = nv}