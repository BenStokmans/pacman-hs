module Map where

import Struct ( Vec2(Vec2), LevelMap(LevelMap), CellType (..), Cell (Cell), Direction(North, East, South, West), allDirections, getCell, getCells, mapWidth, mapHeight, dirToVec2, setCells, scaleVec2 )
import Data.Maybe ( isJust, isNothing )
import Data.List ( intercalate )
import Graphics.Gloss (Picture (..), translate, pictures, circleSolid, thickArc)
import Codec.Picture.Metadata (Keys(Source))
import Rendering (resize)
import Graphics.Gloss.Data.Picture (scale,rectangleSolid)
import Graphics.Gloss.Data.Color ( green, red, white, yellow )
 
getSpawnPoint :: LevelMap -> Vec2
getSpawnPoint (LevelMap w h cs) = foldr (\x c -> c + scaleVec2 x sc) (Vec2 0 0) ss
    where 
        ss = map (\(Cell _ v) -> v) (filter (\(Cell t _) -> t == Spawn) cs)
        sc = 1 / fromIntegral (length ss) :: Float

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

data WallType = StraightOne | OutCorner | InCorner | SingleOutCorner | SingleInVerCorner | SingleInHorCorner | End | StraightTwo | Single | Misc deriving Eq
data WallSection = WallSection WallType Direction

instance Eq WallSection where
    (==) :: WallSection -> WallSection -> Bool
    (WallSection t1 d1) == (WallSection t2 d2) = t1 == t2 && d1 == d2
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


wallToSizedSection :: Float -> Float -> Float -> Float -> WallSection -> Picture
wallToSizedSection m t nw nh ws = let (ow, oh) = defaultSize in resize ow oh nw nh (wallSectionToPic m t ow oh ws)

defaultSize :: (Float, Float)
defaultSize = (100, 100)

-- m = margin, t = thickness, w = width, h = height
wallSectionToPic :: Float -> Float -> Float -> Float -> WallSection -> Picture
wallSectionToPic m t w h (WallSection StraightOne North) = translate 0 ((h/2)-h*m) (rectangleSolid w t)
wallSectionToPic m t w h (WallSection StraightOne East) = translate ((w/2)-w*m) 0 (rectangleSolid t h)
wallSectionToPic m t w h (WallSection StraightOne South) = translate 0 (-(h/2)+h*m) (rectangleSolid w t)
wallSectionToPic m t w h (WallSection StraightOne West) = translate (-(w/2)+w*m) 0 (rectangleSolid t h)
wallSectionToPic m t w h (WallSection OutCorner North) = translate (w/2) (-h/2) $ thickArc 90 180 (w*(1-m)) t
wallSectionToPic m t w h (WallSection OutCorner East) = translate (-w/2) (h/2) $ thickArc (-90) 0 (w*(1-m)) t
wallSectionToPic m t w h (WallSection OutCorner South) = translate (w/2) (h/2) $ thickArc 180 (-90) (w*(1-m)) t
wallSectionToPic m t w h (WallSection OutCorner West) = translate (-w/2) (-h/2) $ thickArc 0 90 (w*(1-m)) t
wallSectionToPic m t w h (WallSection InCorner North) = translate (w/2) (h/2) $ thickArc 180 (-90) (w*m) t
wallSectionToPic m t w h (WallSection InCorner East) = translate (-w/2) (-h/2) $ thickArc 0 90 (w*m) t
wallSectionToPic m t w h (WallSection InCorner South) = translate (-w/2) (h/2) $ thickArc (-90) 0 (w*m) t
wallSectionToPic m t w h (WallSection InCorner West) = translate (w/2) (-h/2) $ thickArc 90 180 (w*m) t
wallSectionToPic m t w h (WallSection End North) = translate 0 (-h/2) $ thickArc 0 180 ((w/2)-w*m) t
-- thickArc does for some reason not see the difference between -90->90 and 90->-90 so we have to mirror it by hand
-- this is because gloss normalizes the angles for a thick arc https://github.com/benl23x5/gloss/gloss-rendering/Graphics/Gloss/Internals/Rendering/Circle.hs#L127C27-L127C41
wallSectionToPic m t w h (WallSection End East) = translate (-w/2) 0 $ scale (-1) 1 $ thickArc (-90) 90 ((w/2)-w*m) t 
wallSectionToPic m t w h (WallSection End South) = translate 0 (h/2) $ thickArc (-180) 0 ((w/2)-w*m) t
wallSectionToPic m t w h (WallSection End West) = translate (w/2) 0 $ thickArc 90 (-90) ((w/2)-w*m) t
wallSectionToPic m t w h (WallSection StraightTwo North) = pictures [translate (-(w/2)+w*m) 0 (rectangleSolid t h),translate ((w/2)-w*m) 0 (rectangleSolid t h)]
wallSectionToPic m t w h (WallSection StraightTwo East) = pictures [translate 0 (-(h/2)+h*m) (rectangleSolid w t),translate 0 ((h/2)-h*m) (rectangleSolid w t)]
wallSectionToPic m t w h (WallSection StraightTwo South) = pictures [translate (-(w/2)+w*m) 0 (rectangleSolid t h),translate ((w/2)-w*m) 0 (rectangleSolid t h)]
wallSectionToPic m t w h (WallSection StraightTwo West) = pictures [translate 0 (-(h/2)+h*m) (rectangleSolid w t),translate 0 ((h/2)-h*m) (rectangleSolid w t)]
wallSectionToPic m t w h (WallSection SingleOutCorner North) = pictures [translate (w/2) (-h/2) $ thickArc 90 180 (w*(1-m)) t, translate (w/2) (-h/2) $ thickArc 90 180 (w*m) t]
wallSectionToPic m t w h (WallSection SingleOutCorner East) = pictures [translate (-w/2) (-h/2) $ thickArc 0 90 (w*(1-m)) t, translate (-w/2) (-h/2) $ thickArc 0 90 (w*m) t]
wallSectionToPic m t w h (WallSection SingleOutCorner South) = pictures [translate (-w/2) (h/2) $ thickArc (-90) 0 (w*(1-m)) t, translate (-w/2) (h/2) $ thickArc (-90) 0 (w*m) t]
wallSectionToPic m t w h (WallSection SingleOutCorner West) = pictures [translate (w/2) (h/2) $ thickArc 180 (-90) (w*(1-m)) t, translate (w/2) (h/2) $ thickArc 180 (-90) (w*m) t]
wallSectionToPic m t w h (WallSection SingleInVerCorner North) = pictures [translate ((w/2)-w*m) 0 (rectangleSolid t h), translate (-w/2) (h/2) $ thickArc (-90) 0 (w*m) t]
wallSectionToPic m t w h (WallSection SingleInVerCorner East) = pictures [translate ((w/2)-w*m) 0 (rectangleSolid t h), translate (-w/2) (-h/2) $ thickArc 0 90 (w*m) t]
wallSectionToPic m t w h (WallSection SingleInVerCorner South) = pictures [translate (-(w/2)+w*m) 0 (rectangleSolid t h), translate (w/2) (h/2) $ thickArc 180 (-90) (w*m) t]
wallSectionToPic m t w h (WallSection SingleInVerCorner West) = pictures [translate (-(w/2)+w*m) 0 (rectangleSolid t h), translate (w/2) (-h/2) $ thickArc 90 180 (w*m) t]
wallSectionToPic m t w h (WallSection SingleInHorCorner North) = pictures [translate 0 ((h/2)-h*m) (rectangleSolid w t), translate (-w/2) (-h/2) $ thickArc 0 90 (w*m) t]
wallSectionToPic m t w h (WallSection SingleInHorCorner East) = pictures [translate 0 ((h/2)-h*m) (rectangleSolid w t), translate (w/2) (-h/2) $ thickArc 90 180 (w*m) t]
wallSectionToPic m t w h (WallSection SingleInHorCorner South) = pictures [translate 0 (-(h/2)+h*m) (rectangleSolid w t), translate (w/2) (h/2) $ thickArc 180 (-90) (w*m) t]
wallSectionToPic m t w h (WallSection SingleInHorCorner West) = pictures [translate 0 (-(h/2)+h*m) (rectangleSolid w t), translate (-w/2) (h/2) $ thickArc (-90) 0 (w*m) t]
wallSectionToPic _ _ _ _ (WallSection _ _) = pictures []

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

--                      NE            SW            SE            NW
diagToWallCorner :: Maybe Cell -> Maybe Cell -> Maybe Cell -> Maybe Cell -> WallSection
diagToWallCorner ne sw se nw
                | isNothing ne && isNothing sw && isNothing se && isJust nw = WallSection OutCorner East
                | isNothing ne && isNothing ne && isJust se && isNothing nw = WallSection OutCorner North
                | isNothing ne && isJust sw && isNothing se && isNothing nw = WallSection OutCorner West
                | isJust ne && isNothing sw && isNothing se && isNothing nw = WallSection OutCorner South
                | isJust ne && isJust sw && isJust se && isNothing nw = WallSection InCorner South
                | isJust ne && isJust sw && isNothing se && isJust nw = WallSection InCorner West
                | isJust ne && isNothing sw && isJust se && isJust nw = WallSection InCorner East
                | isNothing ne && isJust sw && isJust se && isJust nw = WallSection InCorner North
                | otherwise = WallSection Misc North

-- returns ne sw se nw
getDiagsTuple :: LevelMap-> Cell -> (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell)
getDiagsTuple l c@(Cell t pos) = (getCell l (pos + Vec2 1 1), getCell l (pos + Vec2 (-1) (-1)), getCell l (pos + Vec2 1 (-1)), getCell l (pos + Vec2 (-1) 1))

getAdjacentTuple :: LevelMap -> Cell -> (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell)
getAdjacentTuple l c@(Cell t pos) = (getCell l (pos + dirToVec2 North), getCell l (pos + dirToVec2 East), getCell l (pos + dirToVec2 South), getCell l (pos + dirToVec2 West))

singleVert :: LevelMap -> Cell -> Bool
singleVert l c@(Cell t pos) = isNothing (getCell l (pos+dirToVec2 West)) && isJust (getCell l (pos+dirToVec2 North)) && isJust (getCell l (pos+dirToVec2 South)) && isNothing (getCell l (pos+dirToVec2 East))

singleHor :: LevelMap -> Cell -> Bool
singleHor l c@(Cell t pos) = isJust (getCell l (pos+dirToVec2 West)) && isNothing (getCell l (pos+dirToVec2 North)) && isNothing (getCell l (pos+dirToVec2 South)) && isJust (getCell l (pos+dirToVec2 East))

--                                      n         e           s         w              ne         sw         se         nw
remapWallEdges :: WallSection -> (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell) -> (Maybe Cell,Maybe Cell,Maybe Cell,Maybe Cell) -> WallSection
remapWallEdges ws@(WallSection Single _) (n,e,s,w) (ne,sw,se,nw)
            | isJust e && isJust s && isNothing ne && isNothing se = WallSection SingleOutCorner North
            | isJust e && isJust n && isNothing ne && isNothing nw = WallSection SingleOutCorner West
            | isJust w && isJust s && isNothing ne && isNothing nw = WallSection SingleOutCorner East
            | isJust w && isJust n && isNothing ne && isNothing se = WallSection SingleOutCorner South
            | otherwise = ws
remapWallEdges ws@(WallSection StraightOne _) (n,e,s,w) (ne,sw,se,nw)
            | isNothing ne && isNothing e && isNothing se && isNothing nw && isJust w && isJust sw && isJust s && isJust n = WallSection SingleInVerCorner North
            | isNothing ne && isNothing e && isNothing se && isJust nw && isJust w && isNothing sw && isJust s && isJust n = WallSection SingleInVerCorner East
            | isNothing nw && isNothing w && isNothing sw && isNothing ne && isJust e && isJust se && isJust s && isJust n = WallSection SingleInVerCorner South
            | isNothing nw && isNothing w && isNothing sw && isNothing se && isJust e && isJust ne && isJust s && isJust n = WallSection SingleInVerCorner West
            | isNothing nw && isNothing n && isNothing ne && isNothing sw && isJust s && isJust se && isJust e && isJust w = WallSection SingleInHorCorner North
            | isNothing nw && isNothing n && isNothing ne && isJust sw && isJust s && isNothing se && isJust e && isJust w = WallSection SingleInHorCorner East -- !!
            | isNothing sw && isNothing s && isNothing se && isJust nw && isJust n && isNothing ne && isJust e && isJust w = WallSection SingleInHorCorner South -- !!
            | isNothing sw && isNothing s && isNothing se && isNothing nw && isJust n && isJust ne && isJust e && isJust w = WallSection SingleInHorCorner West
            | otherwise = ws
remapWallEdges ws _ _ = ws

mapWallEdges :: LevelMap -> (Cell,WallSection) -> (Cell,WallSection)
mapWallEdges (LevelMap w h cs) x@(c, ws@(WallSection s _)) 
        | s == Single || s == StraightOne = let (a,b) = (getAdjacentTuple (LevelMap w h cs) c, getDiagsTuple (LevelMap w h cs) c) in (c, remapWallEdges ws a b)
        | otherwise = x

processWallGroup :: LevelMap -> [Cell] -> [(Cell,WallSection)]
processWallGroup (LevelMap w h _) cs = mappedCorners ++ mappedWallsEdges
            where
                corners = filter (\c -> let (ld, la) = (length (getDiags (LevelMap w h cs) c),length (getAdjacent (LevelMap w h cs) c)) in (ld == 1 || ld == 3) && (la == 2 || la == 4)) cs
                fcorners = filter (\c -> not (singleVert (LevelMap w h cs) c) && not (singleHor (LevelMap w h cs) c)) corners
                walls = deleteMultiple cs fcorners
                mappedWalls = map (\cell -> let (a,b,c,d) = getAdjacentTuple (LevelMap w h cs) cell in (cell, adjacentToWallSection a b c d)) walls
                mappedCorners = map (\cell -> let (a,b,c,d) = getDiagsTuple (LevelMap w h cs) cell in (cell, diagToWallCorner a b c d)) fcorners
                mappedWallsEdges = map (mapWallEdges (LevelMap w h cs)) mappedWalls

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