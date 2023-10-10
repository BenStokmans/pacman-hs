module Map where

import Types ( Vec2(Vec2), LevelMap(LevelMap), CellType (Empty, Wall, Intersection, Pellet), Cell (Cell), allDirections, getCell, getCells, dirToVec2, setCells )
import Data.Maybe
import Data.List

getDiags :: [Cell] -> Cell -> [Cell]
getDiags l c@(Cell t pos) = getCells (LevelMap l) (map (pos +) [Vec2 1 1, Vec2 (-1) (-1), Vec2 1 (-1), Vec2 (-1) 1])

getAdjacent :: [Cell] -> Cell -> [Cell]
getAdjacent l c@(Cell t pos) = getCells (LevelMap l) (map (\d -> pos + dirToVec2 d) allDirections)

getIntersections :: [Cell] -> [Vec2]
getIntersections l = map (\(Cell _ v) -> v) (filter (\c -> length (emptyAdjacent c) >= 3 && null (emptyDiag c)) l)
                    where
                        emptyAdjacent c = filter (\(Cell t _) -> t == Empty || t == Pellet) (getAdjacent l c)
                        emptyDiag c = filter (\(Cell t _) -> t == Empty || t == Pellet) (getDiags l c)

calculateIntersections :: LevelMap -> LevelMap
calculateIntersections l@(LevelMap m) = setCells l (map (Cell Intersection) (getIntersections m))

getGroup :: [Cell] -> Cell -> [Cell]
getGroup [] _ = []
getGroup l c@(Cell t1 _) = adjacent ++ concatMap (getGroup l') adjacent
                    where
                        adjacent = filter (\(Cell t2 _) -> t1 == t2) (getAdjacent l c)
                        l' = deleteMultiple l adjacent

deleteMultiple :: Eq a => [a] -> [a] -> [a] -- TODO: optimize
deleteMultiple [] _ = []
deleteMultiple (x:xs) ys | x `elem` ys = deleteMultiple xs ys
                         | otherwise = x : deleteMultiple xs ys

getGroups :: [Cell] -> [[Cell]]
getGroups [] = []
getGroups l@(x:xs) = group : getGroups (deleteMultiple xs group)
            where group = x : getGroup l x

getWallGroups :: LevelMap -> [[Cell]]
getWallGroups l@(LevelMap m) = getGroups (filter (\(Cell t _) -> t == Wall) m)

calculateWallGroups :: LevelMap -> LevelMap
calculateWallGroups l@(LevelMap m) = setCells l (map (\(Cell _ v) -> Cell Intersection v) (concat (getWallGroups l)))