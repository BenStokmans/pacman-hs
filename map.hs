module Map where

import Types ( Vec2(Vec2), LevelMap(LevelMap), CellType (Empty, Wall, Intersection, Pellet), Cell (Cell), Direction(North, East, South, West), allDirections, getCell, getCells, dirToVec2, setCells )
import Data.Maybe
import Data.List
import Distribution.Compat.Graph (neighbors)

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

data WallType = StraightOne | OutCorner | InCorner | End | StraightTwo | Single deriving Eq
data WallSection = WallSection WallType Direction

instance Eq WallSection where
    (==) :: WallSection -> WallSection -> Bool
    (WallSection t1 d1) == (WallSection t2 d2) = (t1 == t2) && (d1 == d2)
instance Show WallSection where
    show :: WallSection -> String
    show (WallSection Single _) = "⚀"
    show (WallSection StraightOne North) = "⎺"
    show (WallSection StraightOne East) = "⎜"
    show (WallSection StraightOne South) = "⎽"
    show (WallSection StraightOne West) = "⎟"
    show (WallSection OutCorner North) = "◜"
    show (WallSection OutCorner East) = "◝"
    show (WallSection OutCorner South) = "◞"
    show (WallSection OutCorner West) = "◟"
    show (WallSection InCorner North) = "◞"
    show (WallSection InCorner East) = "◟"
    show (WallSection InCorner South) = "◜"
    show (WallSection InCorner West) = "◝"
    show (WallSection End North) = "⎴"
    show (WallSection End East) = "["
    show (WallSection End South) = "⎵"
    show (WallSection End West) = "]"
    show (WallSection StraightTwo North) = "⎕"
    show (WallSection StraightTwo East) = "⏛"
    show (WallSection StraightTwo South) = "⎕"
    show (WallSection StraightTwo West) = "⏛"
