module GameLogic.Pathfinding where

import Data.List (delete, maximumBy, minimumBy, sortBy)
import Data.Maybe (isJust)
import GameLogic.MapLogic
    ( LevelMap,
      CellType(Wall),
      Vec2(..),
      Direction(..),
      allDirections,
      dirToVec2,
      cellHasType,
      getCell )

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

-- create a new a* node using a provided heuristic function h
newNode :: (Vec2 -> Float) -> AStarNode -> Vec2 -> AStarNode
newNode h from p = AStarNode {pos = p, gCost = gCost', hCost = hCost', fCost = gCost' + hCost', prev = from, dir = dir'}
  where
    gCost' = gCost from + 1
    hCost' = h p
    dir' = getTraveledDirection (pos from) p

isValidPos :: LevelMap -> Vec2 -> Bool
isValidPos m p =
  let mCel = getCell m p
   in isJust mCel && maybe False (not . cellHasType Wall) mCel

-- get vectors adjacent to a provided vector v in all 4 cardinal directions
getAdjacentVecs :: Vec2 -> [Vec2]
getAdjacentVecs v = map (\d -> v + dirToVec2 d) allDirections

-- gets adjacent nodes and applies a heuristic function "h" to calculate the hCost
getAdjacentNodes :: LevelMap -> (Vec2 -> Float) -> AStarNode -> [AStarNode]
getAdjacentNodes m h t@(AStarNode {pos = pos}) = map (newNode h t) (filter (isValidPos m) $ getAdjacentVecs pos)

vec2Dist :: Vec2 -> Vec2 -> Float
vec2Dist (Vec2 x1 y1) (Vec2 x2 y2) = abs ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)) -- we only use this for comparisons so sqrt is not need

-- find node with the smallest fCost
findSmallest :: [AStarNode] -> AStarNode
findSmallest =
  foldl1
    (\c1 c2 ->
       if fCost c1 < fCost c2
         then c1
         else c2)

-- backtracking by coordinates
backtrackVec2 :: AStarNode -> [Vec2]
backtrackVec2 t@(AStarNode {pos = pos, prev = f})
  | f == t = []
  | otherwise = backtrackVec2 f ++ [pos]

-- backtracking by direction
backtrackDir :: AStarNode -> [Direction]
backtrackDir t@(AStarNode {dir = dir, prev = f})
  | f == t = []
  | otherwise = backtrackDir f ++ [dir]

-- partial == True -> always returns a path regardless of whether the goal node is reachable
-- partial == False -> only returns a path if the last node matches the goal node
astar :: (AStarNode -> [a]) -> LevelMap -> Vec2 -> [AStarNode] -> [AStarNode] -> Bool -> Maybe [a]
astar _ _ _ [] _ _ = Nothing
astar f map end open closed partial
  | null open' && partial = Just (f $ minimumBy (\x y -> compare (vec2Dist (pos x) end) (vec2Dist (pos y) end)) closed')
  | pos current == end = Just (f current)
  | otherwise = astar f map end open' closed' partial
  where
    current = findSmallest open
    adjacent = getAdjacentNodes map (vec2Dist end) current
    unexplored = filter (\b -> not (any (b `elem`) [open, closed])) adjacent
    open' = unexplored ++ delete current open
    closed' = current : closed

-- limits path to 3 cardinal directions
astarLimited :: Direction -> (AStarNode -> [a]) -> LevelMap -> Vec2 -> [AStarNode] -> [AStarNode] -> Bool -> Maybe [a]
astarLimited nd f map end open closed partial
  | pos current == end = Just (f current)
  | otherwise = astar f map end open' closed' partial
  where
    current = findSmallest open
    adjacent = filter (\AStarNode {dir = d} -> d /= nd) $ getAdjacentNodes map (vec2Dist end) current
    unexplored = filter (\b -> not (any (b `elem`) [open, closed])) adjacent
    open' = unexplored ++ delete current open
    closed' = current : closed

-- get list of coordinates to the target with the first step being limited to 3 cardinal directions
getPathLimited :: LevelMap -> Direction -> Vec2 -> Vec2 -> Bool -> Maybe [Vec2]
getPathLimited map ad start end = astarLimited ad backtrackVec2 map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

-- get list of directions to the target with the first step being limited to 3 cardinal directions
getDirectionsLimited :: LevelMap -> Direction -> Vec2 -> Vec2 -> Bool -> Maybe [Direction]
getDirectionsLimited map ad start end = astarLimited ad backtrackDir map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

-- get list of coordinates to the target
getPath :: LevelMap -> Vec2 -> Vec2 -> Bool -> Maybe [Vec2]
getPath map start end = astar backtrackVec2 map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start

-- get list of directions to the target
getDirections :: LevelMap -> Vec2 -> Vec2 -> Bool -> Maybe [Direction]
getDirections map start end = astar backtrackDir map end [startCell] []
  where
    startCell = AStarNode {pos = start, fCost = startCost, gCost = 0, hCost = startCost, prev = startCell, dir = North}
    startCost = vec2Dist end start
