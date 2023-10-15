module Map where

import Struct ( Vec2(Vec2), LevelMap(LevelMap), CellType (Empty, Wall, Intersection, Pellet), Cell (Cell), Direction(North, East, South, West), allDirections, getCell, getCells, mapWidth, mapHeight, dirToVec2, setCells )
import Data.Maybe ( isJust, isNothing )
import Data.List ( intercalate )

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

data WallType = StraightOne | OutCorner | InCorner | End | StraightTwo | Single | Misc deriving Eq
data WallSection = WallSection WallType Direction

instance Eq WallSection where
    (==) :: WallSection -> WallSection -> Bool
    (WallSection t1 d1) == (WallSection t2 d2) = (t1 == t2) && (d1 == d2)
instance Show WallSection where
    show :: WallSection -> String
    show (WallSection StraightOne North) = "⎽"
    show (WallSection StraightOne East) = "⎟"
    show (WallSection StraightOne South) = "⎺"
    show (WallSection StraightOne West) = "⎜"
    show (WallSection OutCorner North) = "◟"
    show (WallSection OutCorner East) = "◝"
    show (WallSection OutCorner South) = "◜"
    show (WallSection OutCorner West) = "◞"
    show (WallSection InCorner North) = "◟"
    show (WallSection InCorner East) = "◜"
    show (WallSection InCorner South) = "◞"
    show (WallSection InCorner West) = "◝"
    show (WallSection End North) = "⎴"
    show (WallSection End East) = "]"
    show (WallSection End South) = "⎵"
    show (WallSection End West) = "["
    show (WallSection StraightTwo North) = "⎕"
    show (WallSection StraightTwo East) = "⏛"
    show (WallSection StraightTwo South) = "⎕"
    show (WallSection StraightTwo West) = "⏛"
    show (WallSection Misc _) = "𝗫"
    show (WallSection Single _) = "⚀"

--                          North         East         South          West
adjacentToWallSection :: Maybe Cell -> Maybe Cell -> Maybe Cell -> Maybe Cell -> WallSection
adjacentToWallSection n e s w 
                | isNothing n && isNothing e && isNothing s && isNothing w = WallSection Misc North
                | isNothing n && isJust e && isJust s && isJust w = WallSection StraightOne North
                | isJust n && isNothing e && isJust s && isJust w = WallSection StraightOne East
                | isJust n && isJust e && isNothing s && isJust w = WallSection StraightOne South
                | isJust n && isJust e && isJust s && isNothing w = WallSection StraightOne West
                | isJust n && isNothing e && isJust s && isNothing w = WallSection StraightTwo North
                | isNothing n && isJust e && isNothing s && isJust w = WallSection StraightTwo East
                | isNothing n && isNothing e && isJust s && isNothing w = WallSection End North
                | isNothing n && isNothing e && isNothing s && isJust w = WallSection End East
                | isJust n && isNothing e && isNothing s && isNothing w = WallSection End South
                | isNothing n && isJust e && isNothing s && isNothing w = WallSection End West
                | otherwise = WallSection Single North

--                      NW            NE            SE            SW
diagToWallCorner :: Maybe Cell -> Maybe Cell -> Maybe Cell -> Maybe Cell -> WallSection
diagToWallCorner nw ne se sw 
                | isNothing nw && isNothing ne && isNothing se && isJust sw = WallSection OutCorner East
                | isNothing nw && isNothing ne && isJust se && isNothing sw = WallSection OutCorner North
                | isNothing nw && isJust ne && isNothing se && isNothing sw = WallSection OutCorner West
                | isJust nw && isNothing ne && isNothing se && isNothing sw = WallSection OutCorner South
                | isJust nw && isJust ne && isJust se && isNothing sw = WallSection InCorner South
                | isJust nw && isJust ne && isNothing se && isJust sw = WallSection InCorner West
                | isJust nw && isNothing ne && isJust se && isJust sw = WallSection InCorner East
                | isNothing nw && isJust ne && isJust se && isJust sw = WallSection InCorner North
                | otherwise = WallSection Single North

getDiagsTuple :: [Cell] -> Cell -> (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell)
getDiagsTuple l c@(Cell t pos) = (getCell m (pos + Vec2 1 1), getCell m (pos + Vec2 (-1) (-1)), getCell m (pos + Vec2 1 (-1)), getCell m (pos + Vec2 (-1) 1))
        where m = LevelMap l

getAdjacentTuple :: [Cell] -> Cell -> (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell)
getAdjacentTuple l c@(Cell t pos) = (getCell m (pos + dirToVec2 North), getCell m (pos + dirToVec2 East), getCell m (pos + dirToVec2 South), getCell m (pos + dirToVec2 West))
        where m = LevelMap l

processWallGroup :: [Cell] -> [(Cell,WallSection)]
processWallGroup cs =
    map (\cell -> let (a,b,c,d) = getDiagsTuple cs cell in (cell, diagToWallCorner a b c d)) corners ++
    map (\cell -> let (a,b,c,d) = getAdjacentTuple cs cell in (cell, adjacentToWallSection a b c d)) walls
            where
                corners = filter (\c -> let l = length (getDiags cs c) in l == 1 || l == 3) cs
                walls = deleteMultiple cs corners

showMapWithWalls :: LevelMap -> String
showMapWithWalls m = intercalate "\n" (map unwords cells)
    where
        w = mapWidth m
        h = mapHeight m
        indeces = map (\y -> map (`Vec2` y) [0 .. w]) [0 .. h]
        cells = map (map (maybe "E" getIcon . getCell m)) indeces
        wallGroups = concatMap processWallGroup (getWallGroups m)
        getIcon c@(Cell t _) | t == Pellet || t == Empty = " "
                             | not (null e) = let (_,ws) = head e in show ws
                             | otherwise = show t
            where e = filter (\(cell,_) -> cell == c) wallGroups