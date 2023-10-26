{-# LANGUAGE LambdaCase #-}

module Map where

import Codec.Picture.Metadata (Keys(Source), Value(Double))
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
import Graphics.Gloss (Picture(..), blank, circleSolid, pictures, rotate, thickArc, thickCircle, translate)
import Graphics.Gloss.Data.Color (green, red, white, yellow)
import Graphics.Gloss.Data.Picture (rectangleSolid, scale)
import Rendering (resize)
import Struct
  ( Cell(Cell)
  , CellType(..)
  , Direction(East, North, South, West)
  , GhostType(..)
  , LevelMap(LevelMap)
  , Vec2(Vec2)
  , allDirections
  , cellHasType
  , cellsWithType
  , dirToVec2
  , getCell
  , getCells
  , mapHeight
  , mapWidth
  , outOfBounds
  , scaleVec2
  , setCells
  )

getSpawnPoint' :: LevelMap -> (Cell -> Bool) -> Vec2
getSpawnPoint' (LevelMap w h cs) f
  | null ss = outOfBounds
  | otherwise = foldr (\x c -> c + scaleVec2 x sc) (Vec2 0 0) ss
  where
    ss = map (\(Cell _ v) -> v) (filter f cs)
    sc = 1 / fromIntegral (length ss) :: Float

getSpawnPoint :: LevelMap -> Vec2
getSpawnPoint l = getSpawnPoint' l (cellHasType Spawn)

getGhostSpawnPoint :: LevelMap -> GhostType -> Vec2
getGhostSpawnPoint l gt =
  getSpawnPoint'
    l
    (\case
       (Cell (GhostSpawn t) _) -> t == gt
       _ -> False)

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

deleteMultiple :: Eq a => [a] -> [a] -> [a] -- TODO: optimize
deleteMultiple [] _ = []
deleteMultiple (x:xs) ys
  | x `elem` ys = deleteMultiple xs ys
  | otherwise = x : deleteMultiple xs ys

data WallType
  = StraightOne
  | OutCorner
  | InCorner
  | SingleOutCorner
  | SingleInVerCorner
  | SingleInHorCorner
  | DoubleCorner
  | End
  | StraightTwo
  | StraightGhost
  | GhostInCorner
  | GhostOutCorner
  | Single
  | Misc
  deriving (Eq)

data WallSection =
  WallSection WallType Direction

instance Eq WallSection where
  (==) :: WallSection -> WallSection -> Bool
  (WallSection t1 d1) == (WallSection t2 d2) = t1 == t2 && d1 == d2

wallToSizedSection :: Float -> Float -> Float -> Float -> WallSection -> Picture
wallToSizedSection m t nw nh ws =
  let (ow, oh) = defaultSize
   in resize ow oh nw nh (wallSectionToPic m t ow oh ws)

defaultSize :: (Float, Float)
defaultSize = (100, 100)

dirToAngle :: Direction -> Float
dirToAngle North = 0
dirToAngle East = 90
dirToAngle South = 180
dirToAngle West = 270

wallTypeToPic :: WallType -> Float -> Float -> Float -> Float -> Picture
wallTypeToPic StraightOne m t w h = translate 0 ((h / 2) - h * m) (rectangleSolid w t)
wallTypeToPic OutCorner m t w h = translate (w / 2) (-h / 2) $ thickArc 90 180 (w * (1 - m)) t
wallTypeToPic InCorner m t w h = translate (w / 2) (h / 2) $ thickArc 180 (-90) (w * m) t
wallTypeToPic End m t w h =
  pictures
    [ thickArc 0 180 ((w / 2) - w * m) t
    , translate (-(w / 2) + w * m) (-h / 4) (rectangleSolid t (h / 2))
    , translate ((w / 2) - w * m) (-h / 4) (rectangleSolid t (h / 2))
    ]
wallTypeToPic StraightTwo m t w h =
  pictures [translate (-(w / 2) + w * m) 0 (rectangleSolid t (h * 1.5)), translate ((w / 2) - w * m) 0 (rectangleSolid t h)]
wallTypeToPic StraightGhost m t w h = rectangleSolid t (h * 1.5)
wallTypeToPic GhostInCorner m t w h = translate (w / 4) (-h / 4) $ thickArc 90 180 (w * 0.70 * m) t
wallTypeToPic SingleOutCorner m t w h =
  pictures [translate (w / 2) (-h / 2) $ thickArc 90 180 (w * (1 - m)) t, translate (w / 2) (-h / 2) $ thickArc 90 180 (w * m) t]
wallTypeToPic SingleInVerCorner m t w h =
  pictures [translate ((w / 2) - w * m) 0 (rectangleSolid t h), translate (-w / 2) (h / 2) $ thickArc (-90) 0 (w * m) t]
wallTypeToPic SingleInHorCorner m t w h =
  pictures [translate (-(w / 2) + w * m) 0 (rectangleSolid t h), translate (w / 2) (h / 2) $ thickArc (-180) (-90) (w * m) t]
wallTypeToPic DoubleCorner m t w h =
  pictures
    [ translate 0 ((h / 2) - h * m) (rectangleSolid w t)
    , translate (-w / 2) (-h / 2) $ thickArc 0 90 (w * m) t
    , translate (w / 2) (-h / 2) $ thickArc 90 180 (w * m) t
    ]
-- wallTypeToPic Single         m t w _ = thickCircle (w*m) t
wallTypeToPic _ _ _ _ _ = blank

-- m = margin, t = thickness, w = width, h = height
wallSectionToPic :: Float -> Float -> Float -> Float -> WallSection -> Picture
wallSectionToPic m t w h (WallSection wt dir) = rotate (dirToAngle dir) (wallTypeToPic wt m t w h)

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
  | isNothing ne && isNothing sw && isNothing se && isJust nw = WallSection OutCorner South
  | isNothing ne && isNothing ne && isJust se && isNothing nw = WallSection OutCorner North
  | isNothing ne && isJust sw && isNothing se && isNothing nw = WallSection OutCorner East
  | isJust ne && isNothing sw && isNothing se && isNothing nw = WallSection OutCorner West
  | isJust ne && isJust sw && isJust se && isNothing nw = WallSection InCorner West
  | isJust ne && isJust sw && isNothing se && isJust nw = WallSection InCorner East
  | isJust ne && isNothing sw && isJust se && isJust nw = WallSection InCorner South
  | isNothing ne && isJust sw && isJust se && isJust nw = WallSection InCorner North
  | otherwise = WallSection Single North

-- returns ne sw se nw
getDiagsTuple :: LevelMap -> Cell -> (Maybe Cell, Maybe Cell, Maybe Cell, Maybe Cell)
getDiagsTuple l c@(Cell t pos) =
  (getCell l (pos + Vec2 1 1), getCell l (pos + Vec2 (-1) (-1)), getCell l (pos + Vec2 1 (-1)), getCell l (pos + Vec2 (-1) 1))

getAdjacentTuple :: LevelMap -> Cell -> (Maybe Cell, Maybe Cell, Maybe Cell, Maybe Cell)
getAdjacentTuple l c@(Cell t pos) =
  (getCell l (pos + dirToVec2 North), getCell l (pos + dirToVec2 East), getCell l (pos + dirToVec2 South), getCell l (pos + dirToVec2 West))

singleVert :: LevelMap -> Cell -> Bool
singleVert l c@(Cell t pos) =
  isNothing (getCell l (pos + dirToVec2 West)) &&
  isJust (getCell l (pos + dirToVec2 North)) && isJust (getCell l (pos + dirToVec2 South)) && isNothing (getCell l (pos + dirToVec2 East))

singleHor :: LevelMap -> Cell -> Bool
singleHor l c@(Cell t pos) =
  isJust (getCell l (pos + dirToVec2 West)) &&
  isNothing (getCell l (pos + dirToVec2 North)) && isNothing (getCell l (pos + dirToVec2 South)) && isJust (getCell l (pos + dirToVec2 East))

remapWallEdges :: WallSection -> (Maybe Cell, Maybe Cell, Maybe Cell, Maybe Cell) -> (Maybe Cell, Maybe Cell, Maybe Cell, Maybe Cell) -> WallSection
remapWallEdges ws@(WallSection Single _) (n, e, s, w) (ne, sw, se, nw)
  | isNothing n && isNothing e && isNothing s && isNothing w && isNothing ne && isNothing sw && isNothing se && isNothing ne = ws
  | isJust e && isJust s && isNothing ne && isNothing se = WallSection SingleOutCorner North
  | isJust e && isJust n && isNothing ne && isNothing nw = WallSection SingleOutCorner West
  | isJust w && isJust s && isNothing ne && isNothing nw = WallSection SingleOutCorner East
  | isJust w && isJust n && isNothing ne && isNothing se = WallSection SingleOutCorner South
  | otherwise = ws
remapWallEdges ws@(WallSection StraightOne _) (n, e, s, w) (ne, sw, se, nw)
  | isNothing nw && isNothing n && isNothing ne && isNothing sw && isJust s && isNothing se && isJust e && isJust w = WallSection DoubleCorner North
  | isNothing ne && isNothing e && isNothing se && isNothing nw && isJust w && isNothing sw && isJust n && isJust s = WallSection DoubleCorner East
  | isNothing sw && isNothing s && isNothing se && isNothing nw && isJust n && isNothing ne && isJust e && isJust w = WallSection DoubleCorner South
  | isNothing nw && isNothing w && isNothing sw && isNothing ne && isJust e && isNothing se && isJust n && isJust s = WallSection DoubleCorner West
  | isNothing ne && isNothing e && isNothing se && isNothing nw && isJust w && isJust sw && isJust s && isJust n = WallSection SingleInVerCorner North
  | isNothing ne && isNothing e && isNothing se && isJust nw && isJust w && isNothing sw && isJust s && isJust n = WallSection SingleInHorCorner South
  | isNothing nw && isNothing w && isNothing sw && isNothing ne && isJust e && isJust se && isJust s && isJust n = WallSection SingleInHorCorner North
  | isNothing nw && isNothing w && isNothing sw && isNothing se && isJust e && isJust ne && isJust s && isJust n = WallSection SingleInVerCorner South
  | isNothing nw && isNothing n && isNothing ne && isNothing sw && isJust s && isJust se && isJust e && isJust w = WallSection SingleInVerCorner West
  | isNothing nw && isNothing n && isNothing ne && isJust sw && isJust s && isNothing se && isJust e && isJust w = WallSection SingleInHorCorner East
  | isNothing sw && isNothing s && isNothing se && isJust nw && isJust n && isNothing ne && isJust e && isJust w = WallSection SingleInVerCorner East
  | isNothing sw && isNothing s && isNothing se && isNothing nw && isJust n && isJust ne && isJust e && isJust w = WallSection SingleInHorCorner West
  | otherwise = ws
remapWallEdges ws _ _ = ws

mapWallEdges :: LevelMap -> (Cell, WallSection) -> (Cell, WallSection)
mapWallEdges (LevelMap w h cs) x@(c, ws@(WallSection s _))
  | s == Single || s == StraightOne =
    let (a, b) = (getAdjacentTuple (LevelMap w h cs) c, getDiagsTuple (LevelMap w h cs) c)
     in (c, remapWallEdges ws a b)
  | otherwise = x

processWalls' :: LevelMap -> [Cell] -> [(Cell, WallSection)]
processWalls' (LevelMap width height _) cs = mappedCorners ++ mappedWallsEdges
  where
    corners =
      filter
        (\c ->
           let (ld, la) = (length (getDiags (LevelMap width height cs) c), length (getAdjacent (LevelMap width height cs) c))
            in (ld == 1 || ld == 3) && (la == 2 || la == 4))
        cs
    fcorners = filter (\c -> not (singleVert (LevelMap width height cs) c) && not (singleHor (LevelMap width height cs) c)) corners
    walls = deleteMultiple cs fcorners
    mappedWalls =
      map
        (\cell ->
           let (n, e, s, w) = getAdjacentTuple (LevelMap width height cs) cell
            in (cell, adjacentToWallSection n e s w))
        walls
    mappedCorners =
      map
        (\cell ->
           let (ne, sw, se, nw) = getDiagsTuple (LevelMap width height cs) cell
            in (cell, diagToWallCorner ne sw se nw))
        fcorners
    mappedWallsEdges = map (mapWallEdges (LevelMap width height cs)) mappedWalls

wallToGhostWall :: WallType -> WallType
wallToGhostWall StraightTwo = StraightGhost
wallToGhostWall End = StraightGhost
wallToGhostWall SingleOutCorner = GhostInCorner
wallToGhostWall _ = Single

processGhostWalls :: LevelMap -> [(Cell, WallSection)]
processGhostWalls m@(LevelMap _ _ l) =
  map (\(c, WallSection t d) -> (c, WallSection (wallToGhostWall t) d)) $ processWalls' m (cellsWithType GhostWall l)

processWalls :: LevelMap -> [(Cell, WallSection)]
processWalls m@(LevelMap _ _ l) = processWalls' m (cellsWithType Wall l) ++ processGhostWalls m
