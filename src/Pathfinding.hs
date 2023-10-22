module Pathfinding where

import           Data.List  (delete)
import           Data.Maybe (isJust)
import           Struct     (Cell (Cell), CellType (Empty, Intersection, Wall),
                             Direction (East, North, South, West),
                             LevelMap (LevelMap), Vec2 (Vec2), allDirections,
                             dirToVec2, getCell, mapHeight, mapWidth, setCells,
                             stringToCellType)

data AStarCell = AStarCell { pos :: Vec2, fCost :: Float, gCost :: Float, hCost :: Float, prev :: AStarCell, dir :: Direction }

instance Eq AStarCell where
  (==) :: AStarCell -> AStarCell -> Bool
  a == b = pos a == pos b

getTraveledDirection :: Vec2 -> Vec2 -> Direction
getTraveledDirection (Vec2 x1 y1) (Vec2 x2 y2) | x1 == x2     && y1 - 1 == y2 = North
                                               | x1 == x2     && y1 + 1 == y2 = South
                                               | x1 - 1 == x2 && y1 == y2     = West
                                               | x1 + 1 == x2 && y1 == y2     = East

newCell :: (Vec2 -> Float) -> AStarCell -> Vec2 -> AStarCell
newCell h from p = AStarCell {
                      pos = p,
                      gCost = gCost',
                      hCost = hCost',
                      fCost = gCost' + hCost',
                      prev = from,
                      dir = dir'
                    }
  where gCost' = gCost from + 1
        hCost' = h p
        dir' = getTraveledDirection (pos from) p

isValidPos :: LevelMap -> Vec2 -> Bool
isValidPos m p = isJust (getCell m p) && maybe False (\(Cell t _) -> t /= Wall) (getCell m p)

getAdjacent :: LevelMap -> (Vec2 -> Float) -> AStarCell -> [AStarCell]
getAdjacent m h t@(AStarCell { pos = pos }) = map (newCell h t) (filter (isValidPos m) (map (\d -> pos + dirToVec2 d) allDirections))

vec2Dist :: Vec2 -> Vec2 -> Float
vec2Dist (Vec2 x1 y1) (Vec2 x2 y2) = abs ((x1 - x2) + (y1 - y2))

findSmallest :: [AStarCell] -> AStarCell
findSmallest = foldl1 (\c1 c2 -> if fCost c1 < fCost c2 then c1 else c2)

backtrack :: AStarCell -> [Vec2]
backtrack t@(AStarCell { pos = pos, prev = f }) | f == t = []
                                                | otherwise = backtrack f ++ [pos]

astar :: LevelMap -> Vec2 -> [AStarCell] -> [AStarCell] -> Maybe [Vec2]
astar _ _ [] _ = Nothing
astar map end open closed | pos current == end = Just (backtrack current)
                          | otherwise = astar map end open' closed'
                        where
                          current = findSmallest open
                          adjacent = getAdjacent map (vec2Dist end) current
                          unexplored = filter (\b -> not (any (b`elem`) [open, closed])) adjacent
                          open' = unexplored ++ delete current open
                          closed' = current : closed

getShortestPath :: LevelMap -> Vec2 -> Vec2 -> Maybe [Vec2]
getShortestPath map start end = astar map end [startCell] []
  where startCell = AStarCell {
                        pos = start,
                        fCost = startCost,
                        gCost = 0,
                        hCost = startCost,
                        prev = startCell,
                        dir = North
                      }
        startCost = vec2Dist end start
