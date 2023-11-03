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
      GhostActor(gUpdate, gCurrentBehaviour, ghostType, gDirection,
                 gLocation, gTarget),
      GhostBehaviour(Frightened, Scatter, Chase),
      GhostType(..),
      LevelMap(..),
      Player(pDirection, pLocation),
      Vec2(..) )
import Graphics.Gloss (Point)
import State (GlobalState (..), GameState (..), gameGridDimensions, gameGridInfo)
import Map (deleteMultiple)
import Pathfinding
import Rendering
import Data.Maybe
import Data.List (minimumBy)
import Control.Monad.Random (getRandomR)


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
