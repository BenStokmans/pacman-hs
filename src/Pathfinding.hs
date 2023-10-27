module Pathfinding where

import Data.List (delete)
import Data.Maybe (isJust)
import Struct
  ( Cell(Cell)
  , CellType(Empty, Intersection, Wall)
  , Direction(East, North, South, West)
  , LevelMap(LevelMap)
  , Vec2(Vec2)
  , allDirections
  , dirToVec2
  , getCell
  , mapHeight
  , mapWidth
  , setCells
  , stringToCellType, cellHasType
  )

data AStarNode = AStarNode
  { pos :: Vec2
  , fCost :: Float
  , gCost :: Float
  , hCost :: Float
  , prev :: AStarNode
  , dir :: Direction
  }

instance Eq AStarNode where
  (==) :: AStarNode -> AStarNode -> Bool
  a == b = pos a == pos b

getTraveledDirection :: Vec2 -> Vec2 -> Direction
getTraveledDirection (Vec2 x1 y1) (Vec2 x2 y2)
  | x1 == x2 && y1 - 1 == y2 = North
  | x1 == x2 && y1 + 1 == y2 = South
  | x1 - 1 == x2 && y1 == y2 = West
  | x1 + 1 == x2 && y1 == y2 = East

newCell :: (Vec2 -> Float) -> AStarNode -> Vec2 -> AStarNode
newCell h from p = AStarNode {pos = p, gCost = gCost', hCost = hCost', fCost = gCost' + hCost', prev = from, dir = dir'}
  where
    gCost' = gCost from + 1
    hCost' = h p
    dir' = getTraveledDirection (pos from) p

isValidPos :: LevelMap -> Vec2 -> Bool
isValidPos m p = let mCel = getCell m p in isJust mCel && maybe False (not . cellHasType Wall) mCel

getAdjacent :: LevelMap -> (Vec2 -> Float) -> AStarNode -> [AStarNode]
getAdjacent m h t@(AStarNode {pos = pos}) = map (newCell h t) (filter (isValidPos m) (map (\d -> pos + dirToVec2 d) allDirections))

vec2Dist :: Vec2 -> Vec2 -> Float
vec2Dist (Vec2 x1 y1) (Vec2 x2 y2) = abs ((x1 - x2) + (y1 - y2))

findSmallest :: [AStarNode] -> AStarNode
findSmallest =
  foldl1
    (\c1 c2 ->
       if fCost c1 < fCost c2
         then c1
         else c2)

backtrackVec2 :: AStarNode -> [Vec2]
backtrackVec2 t@(AStarNode {pos = pos, prev = f})
  | f == t = []
  | otherwise = backtrackVec2 f ++ [pos]

backtrackDir :: AStarNode -> [Direction]
backtrackDir t@(AStarNode {dir = dir, prev = f})
  | f == t = []
  | otherwise = backtrackDir f ++ [dir]

astar' :: (AStarNode -> [a]) -> LevelMap -> Vec2 -> [AStarNode] -> [AStarNode] -> Maybe [a]
astar' _ _ _ [] _ = Nothing
astar' f map end open closed
  | pos current == end = Just (f current)
  | otherwise = astar' f map end open' closed'
  where
    current = findSmallest open
    adjacent = getAdjacent map (vec2Dist end) current
    unexplored = filter (\b -> not (any (b `elem`) [open, closed])) adjacent
    open' = unexplored ++ delete current open
    closed' = current : closed

astarLim' :: Direction -> (AStarNode -> [a]) -> LevelMap -> Vec2 -> [AStarNode] -> [AStarNode] -> Maybe [a]
astarLim' nd f map end open closed | pos current == end = Just (f current)
  | otherwise = astar' f map end open' closed'
  where
    current = findSmallest open
    adjacent = filter (\AStarNode {dir = d} -> d /= nd) $ getAdjacent map (vec2Dist end) current
    unexplored = filter (\b -> not (any (b `elem`) [open, closed])) adjacent
    open' = unexplored ++ delete current open
    closed' = current : closed

getShortestPathLim :: LevelMap -> Direction -> Vec2 -> Vec2 -> Maybe [Vec2]
getShortestPathLim map ad start end = astarLim' ad backtrackVec2 map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

getShortestDirectionsLim :: LevelMap -> Direction -> Vec2 -> Vec2 -> Maybe [Direction]
getShortestDirectionsLim map ad start end = astarLim' ad backtrackDir map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

getShortestPath :: LevelMap -> Vec2 -> Vec2 -> Maybe [Vec2]
getShortestPath map start end = astar' backtrackVec2 map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

getShortestDirections :: LevelMap -> Vec2 -> Vec2 -> Maybe [Direction]
getShortestDirections map start end = astar' backtrackDir map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start