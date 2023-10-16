module Map where

import Struct ( Vec2(Vec2), LevelMap(LevelMap), CellType (Empty, Wall, Intersection, Pellet), Cell (Cell), Direction(North, East, South, West), allDirections, getCell, getCells, mapWidth, mapHeight, dirToVec2, setCells )
import Data.Maybe ( isJust, isNothing )
import Data.List ( intercalate )
import Graphics.Gloss (Picture (..), translate, pictures, circleSolid)
import Graphics.Gloss.Data.Color
import Codec.Picture.Metadata (Keys(Source))

getDiags :: LevelMap -> Cell -> [Cell]
getDiags l c@(Cell t pos) = getCells l (map (pos +) [Vec2 1 1, Vec2 (-1) (-1), Vec2 1 (-1), Vec2 (-1) 1])

getAdjacent :: LevelMap -> Cell -> [Cell]
getAdjacent l c@(Cell t pos) = getCells l (map (\d -> pos + dirToVec2 d) allDirections)

getIntersections :: LevelMap -> [Vec2]
getIntersections l@(LevelMap _ _ m) = map (\(Cell _ v) -> v) (filter (\c -> length (emptyAdjacent c) >= 3 && null (emptyDiag c)) m)
                    where
                        emptyAdjacent c = filter (\(Cell t _) -> t == Empty || t == Pellet) (getAdjacent l c)
                        emptyDiag c = filter (\(Cell t _) -> t == Empty || t == Pellet) (getDiags l c)

calculateIntersections :: LevelMap -> LevelMap
calculateIntersections l = setCells l (map (Cell Intersection) (getIntersections l))

getGroup :: LevelMap -> Cell -> [Cell]
getGroup (LevelMap _ _ []) _ = []
getGroup l@(LevelMap w h m) c@(Cell t1 _) = adjacent ++ concatMap (getGroup l') adjacent
                    where
                        adjacent = filter (\(Cell t2 _) -> t1 == t2) (getAdjacent l c)
                        l' = LevelMap w h (deleteMultiple m adjacent)

deleteMultiple :: Eq a => [a] -> [a] -> [a] -- TODO: optimize
deleteMultiple [] _ = []
deleteMultiple (x:xs) ys | x `elem` ys = deleteMultiple xs ys
                         | otherwise = x : deleteMultiple xs ys

getGroups :: LevelMap -> [[Cell]]
getGroups (LevelMap _ _ []) = []
getGroups l@(LevelMap w h (x:xs)) = group : getGroups (LevelMap w h (deleteMultiple xs group))
            where group = x : getGroup l x

getWallGroups :: LevelMap -> [[Cell]]
getWallGroups (LevelMap w h m) = getGroups (LevelMap w h (filter (\(Cell t _) -> t == Wall) m))

calculateWallGroups :: LevelMap -> LevelMap
calculateWallGroups l@(LevelMap _ _ m) = setCells l (map (\(Cell _ v) -> Cell Intersection v) (concat (getWallGroups l)))

data WallType = StraightOne | OutCorner | InCorner | SingleCorner | End | StraightTwo | Single | Misc deriving Eq
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

wallSectionToPic :: Float -> Float -> WallSection -> Picture
wallSectionToPic w h (WallSection StraightOne North) = Color white $ Line [(-w/2,h/16),(w/2,h/16)]
wallSectionToPic w h (WallSection StraightOne East) = Color white $ Line [(0,h/2),(0,-h/2)]
wallSectionToPic w h (WallSection StraightOne South) = Color white $ Line [(-w/2,-h/16),(w/2,-h/16)]
wallSectionToPic w h (WallSection StraightOne West) = Color white $ Line [(0,h/2),(0,-h/2)]
wallSectionToPic w h (WallSection OutCorner North) = Color white $ translate (w/2) (-h/2) $ Arc 90 180 (w/2)
wallSectionToPic w h (WallSection OutCorner East) = Color white $ translate (-w/2) (h/2) $ Arc (-90) 0 (w/2)
wallSectionToPic w h (WallSection OutCorner South) = Color white $ translate (w/2) (h/2) $ Arc (180) (-90) (w/2)
wallSectionToPic w h (WallSection OutCorner West) = Color white $ translate (-w/2) (-h/2) $ Arc (0) (90) (w/2)
wallSectionToPic w h (WallSection InCorner North) = Color white $ translate (w/2) (h/2+h/16) $ Arc (180) (-90) (w/2)
wallSectionToPic w h (WallSection InCorner East) = Color white $ translate (-w/2) (-h/2-h/8) $ Arc (0) (90) (w/2)
wallSectionToPic w h (WallSection InCorner South) = Color white $ translate (-w/2) (h/2+h/16) $ Arc (-90) 0 (w/2)
wallSectionToPic w h (WallSection InCorner West) = Color white $ translate (w/2) (-h/2-h/8) $ Arc (90) (180) (w/2)
wallSectionToPic w h (WallSection End North) = Color white $ Arc (90) (90) (w/4) -- !!
wallSectionToPic w h (WallSection End East) = Color white $ translate (-w/2) 0 $ Arc (-90) (90) (w/4)
wallSectionToPic w h (WallSection End South) = Color white $ Arc (90) (90) (w/4) -- !!
wallSectionToPic w h (WallSection End West) = Color white $ translate (w/2) 0 $ Arc (90) (-90) (w/4)
wallSectionToPic w h (WallSection StraightTwo North) = Color white $ pictures [Line [(-w/4,h/2),(-w/4,-h/2)],Line [(w/4,h/2),(w/4,-h/2)]] -- !!
wallSectionToPic w h (WallSection StraightTwo East) = Color white $ pictures [Line [(-w/2,-h/4),(w/2,-h/4)],Line [(-w/2,h/4),(w/2,h/4)]]
wallSectionToPic w h (WallSection StraightTwo South) = Color white $ pictures [Line [(-w/4,h/2),(-w/4,-h/2)],Line [(w/4,h/2),(w/4,-h/2)]] -- !!
wallSectionToPic w h (WallSection StraightTwo West) = Color white $ pictures [Line [(-w/2,-h/4),(w/2,-h/4)],Line [(-w/2,h/4),(w/2,h/4)]]
wallSectionToPic w h (WallSection SingleCorner North) = Color white $ pictures [translate (w/2-w/16) (-h/2) $ Arc 90 180 (w/1.5), translate (w/2) (-h/2) $ Arc 90 180 (w/4)]
wallSectionToPic w h (WallSection SingleCorner East) = Color white $ pictures [translate (-w/2+w/16) (-h/2) $ Arc 0 90 (w/1.5), translate (-w/2) (-h/2) $ Arc 0 90 (w/4)]
wallSectionToPic w h (WallSection SingleCorner South) = Color white $ pictures [translate (-w/2+w/16) (h/2) $ Arc (-90) 0 (w/1.5), translate (-w/2) (h/2) $ Arc (-90) 0 (w/4)]
wallSectionToPic w h (WallSection SingleCorner West) = Color white $ pictures [translate (w/2-w/16) (h/2) $ Arc (180) (-90) (w/1.5), translate (w/2) (h/2) $ Arc (180) (-90) (w/4)]
wallSectionToPic w h (WallSection Misc _) = Color white $ circleSolid (w/2)
wallSectionToPic w h (WallSection Single _) = Color white $ circleSolid (w/2)
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
                | otherwise = WallSection Misc North

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

getDiagsTuple :: LevelMap-> Cell -> (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell)
getDiagsTuple l c@(Cell t pos) = (getCell l (pos + Vec2 1 1), getCell l (pos + Vec2 (-1) (-1)), getCell l (pos + Vec2 1 (-1)), getCell l (pos + Vec2 (-1) 1))

getAdjacentTuple :: LevelMap -> Cell -> (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell)
getAdjacentTuple l c@(Cell t pos) = (getCell l (pos + dirToVec2 North), getCell l (pos + dirToVec2 East), getCell l (pos + dirToVec2 South), getCell l (pos + dirToVec2 West))

singleVert :: LevelMap -> Cell -> Bool
singleVert l c@(Cell t pos) = isNothing (getCell l (pos+dirToVec2 West)) && isJust (getCell l (pos+dirToVec2 North)) && isJust (getCell l (pos+dirToVec2 South)) && isNothing (getCell l (pos+dirToVec2 East))

singleHor :: LevelMap -> Cell -> Bool
singleHor l c@(Cell t pos) = isJust (getCell l (pos+dirToVec2 West)) && isNothing (getCell l (pos+dirToVec2 North)) && isNothing (getCell l (pos+dirToVec2 South)) && isJust (getCell l (pos+dirToVec2 East))

--                      n         e           s         w              nw         ne         se         sw
identifySingle :: (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell) -> (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell) -> WallSection
identifySingle (n,e,s,w) (nw,ne,se,sw)
            | isJust e && isJust s && isNothing nw && isNothing se = WallSection SingleCorner North
            | isJust e && isJust n && isNothing nw && isNothing sw = WallSection SingleCorner West
            | isJust w && isJust s && isNothing nw && isNothing sw = WallSection SingleCorner East
            | isJust w && isJust n && isNothing nw && isNothing se = WallSection SingleCorner South
            | otherwise = WallSection Single North


processWallGroup :: LevelMap -> [Cell] -> [(Cell,WallSection)]
processWallGroup (LevelMap w h _) cs = mappedCorners ++ remappedWalls
            where
                corners = filter (\c -> let (ld, la) = (length (getDiags (LevelMap w h cs) c),length (getAdjacent (LevelMap w h cs) c)) in (ld == 1 || ld == 3) && (la == 2 || la == 4)) cs
                fcorners = filter (\c -> not (singleVert (LevelMap w h cs) c) && not (singleHor (LevelMap w h cs) c)) corners
                walls = deleteMultiple cs fcorners
                mappedWalls = map (\cell -> let (a,b,c,d) = getAdjacentTuple (LevelMap w h cs) cell in (cell, adjacentToWallSection a b c d)) walls
                mappedCorners = map (\cell -> let (a,b,c,d) = getDiagsTuple (LevelMap w h cs) cell in (cell, diagToWallCorner a b c d)) fcorners
                remappedWalls = map (\s@(c,WallSection t _) -> if t == Misc then let (a,b) = (getAdjacentTuple (LevelMap w h cs) c, getDiagsTuple (LevelMap w h cs) c) in (c, identifySingle a b) else s) mappedWalls
processWallGroups :: LevelMap -> [[(Cell,WallSection)]]
processWallGroups m = map (processWallGroup m) (getWallGroups m)

showMapWithWalls :: LevelMap -> String
showMapWithWalls m@(LevelMap w h l) = intercalate "\n" (map unwords cells)
    where
        indeces = map (\y -> map (`Vec2` y) [0 .. w]) [0 .. h]
        cells = map (map (maybe "E" getIcon . getCell m)) indeces
        wallGroups = concat $ processWallGroups m
        getIcon c@(Cell t _) | t == Pellet || t == Empty = " "
                             | not (null e) = let (_,ws) = head e in show ws
                             | otherwise = show t
            where e = filter (\(cell,_) -> cell == c) wallGroups