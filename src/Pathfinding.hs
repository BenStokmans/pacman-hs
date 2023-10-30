module Pathfinding where

import Data.List (delete, maximumBy, minimumBy, sortBy)
import Data.Maybe (isJust)
import Struct
  ( Cell(Cell)
  , CellType(Empty, Intersection, Wall)
  , Direction(East, North, South, West)
  , LevelMap(LevelMap)
  , Vec2(Vec2)
  , allDirections
  , cellHasType
  , dirToVec2
  , getCell
  , mapHeight
  , mapWidth
  , oppositeDirection
  , setCells
  , stringToCellType
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
  | x1 == x2 && y1 - 1 == y2 = South
  | x1 == x2 && y1 + 1 == y2 = North
  | x1 - 1 == x2 && y1 == y2 = West
  | x1 + 1 == x2 && y1 == y2 = East

newCell :: (Vec2 -> Float) -> AStarNode -> Vec2 -> AStarNode
newCell h from p = AStarNode {pos = p, gCost = gCost', hCost = hCost', fCost = gCost' + hCost', prev = from, dir = dir'}
  where
    gCost' = gCost from + 1
    hCost' = h p
    dir' = getTraveledDirection (pos from) p

isValidPos :: LevelMap -> Vec2 -> Bool
isValidPos m p =
  let mCel = getCell m p
   in isJust mCel && maybe False (not . cellHasType Wall) mCel

getAdjacent :: LevelMap -> (Vec2 -> Float) -> AStarNode -> [AStarNode]
getAdjacent m h t@(AStarNode {pos = pos}) = map (newCell h t) (filter (isValidPos m) (map (\d -> pos + dirToVec2 d) allDirections))

vec2Dist :: Vec2 -> Vec2 -> Float
vec2Dist (Vec2 x1 y1) (Vec2 x2 y2) = abs ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)) -- we only use this for comparisons so sqrt is not need

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

-- TODO: cleanup everything below
-- always returns a path regardless of whether the goal node is reacable
astarPartial :: (AStarNode -> [a]) -> LevelMap -> Vec2 -> [AStarNode] -> [AStarNode] -> Maybe [a]
astarPartial f map end open closed
  | null open' = Just (f $ minimumBy (\x y -> compare (vec2Dist (pos x) end) (vec2Dist (pos y) end)) closed')
  | pos current == end = Just (f current)
  | otherwise = astarPartial f map end open' closed'
  where
    current = findSmallest open
    adjacent = getAdjacent map (vec2Dist end) current
    unexplored = filter (\b -> not (any (b `elem`) [open, closed])) adjacent
    open' = unexplored ++ delete current open
    closed' = current : closed

-- only returns a path if the last node matches the goal node
astarComplete :: (AStarNode -> [a]) -> LevelMap -> Vec2 -> [AStarNode] -> [AStarNode] -> Maybe [a]
astarComplete _ _ _ [] _ = Nothing
astarComplete f map end open closed
  | pos current == end = Just (f current)
  | otherwise = astarComplete f map end open' closed'
  where
    current = findSmallest open
    adjacent = getAdjacent map (vec2Dist end) current
    unexplored = filter (\b -> not (any (b `elem`) [open, closed])) adjacent
    open' = unexplored ++ delete current open
    closed' = current : closed

-- limits path to 3 cardinal directions
astarLimComplete :: Direction -> (AStarNode -> [a]) -> LevelMap -> Vec2 -> [AStarNode] -> [AStarNode] -> Maybe [a]
astarLimComplete nd f map end open closed
  | pos current == end = Just (f current)
  | otherwise = astarComplete f map end open' closed'
  where
    current = findSmallest open
    adjacent = filter (\AStarNode {dir = d} -> d /= nd) $ getAdjacent map (vec2Dist end) current
    unexplored = filter (\b -> not (any (b `elem`) [open, closed])) adjacent
    open' = unexplored ++ delete current open
    closed' = current : closed

astarLimPartial :: Direction -> (AStarNode -> [a]) -> LevelMap -> Vec2 -> [AStarNode] -> [AStarNode] -> Maybe [a]
astarLimPartial nd f map end open closed
  | pos current == end = Just (f current)
  | otherwise = astarPartial f map end open' closed'
  where
    current = findSmallest open
    adjacent = filter (\AStarNode {dir = d} -> d /= nd) $ getAdjacent map (vec2Dist end) current
    unexplored = filter (\b -> not (any (b `elem`) [open, closed])) adjacent
    open' = unexplored ++ delete current open
    closed' = current : closed

getShortestPathLimComplete :: LevelMap -> Direction -> Vec2 -> Vec2 -> Maybe [Vec2]
getShortestPathLimComplete map ad start end = astarLimComplete ad backtrackVec2 map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

getShortestDirectionsLimComplete :: LevelMap -> Direction -> Vec2 -> Vec2 -> Maybe [Direction]
getShortestDirectionsLimComplete map ad start end = astarLimComplete ad backtrackDir map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

getShortestPathLimPartial :: LevelMap -> Direction -> Vec2 -> Vec2 -> Maybe [Vec2]
getShortestPathLimPartial map ad start end = astarLimPartial ad backtrackVec2 map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

getShortestDirectionsLimPartial :: LevelMap -> Direction -> Vec2 -> Vec2 -> Maybe [Direction]
getShortestDirectionsLimPartial map ad start end = astarLimPartial ad backtrackDir map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

getShortestPath :: LevelMap -> Vec2 -> Vec2 -> Maybe [Vec2]
getShortestPath map start end = astarComplete backtrackVec2 map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

getShortestDirections :: LevelMap -> Vec2 -> Vec2 -> Maybe [Direction]
getShortestDirections map start end = astarComplete backtrackDir map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start
