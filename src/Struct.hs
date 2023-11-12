module Struct where

import Data.Aeson
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Graphics.Gloss (Point)

type GridInfo = ((Float, Float), (Float, Float))

data Direction
  = North
  | South
  | West
  | East
  deriving (Eq, Show)

tailNull :: [a] -> [a]
tailNull xs | null xs = []
            | otherwise = tail xs

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just $ head xs

allDirections :: [Direction]
allDirections = [North, East, South, West]

adjacentVecs :: Vec2 -> [Vec2]
adjacentVecs v = map (\d -> v + dirToVec2 d) allDirections

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East = West
oppositeDirection South = North
oppositeDirection West = East

data Vec2 =
  Vec2 Float Float

outOfBounds :: Vec2
outOfBounds = Vec2 (-1) (-1)

instance Show Vec2 where
  show :: Vec2 -> String
  show (Vec2 x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Vec2 where
  (==) :: Vec2 -> Vec2 -> Bool
  (Vec2 x1 y1) == (Vec2 x2 y2) = (x1 == x2) && (y1 == y2)

instance Num Vec2 where
  negate :: Vec2 -> Vec2
  negate (Vec2 x y) = Vec2 (-x) (-y)
  (+) :: Vec2 -> Vec2 -> Vec2
  (Vec2 x1 y1) + (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
  (-) :: Vec2 -> Vec2 -> Vec2
  (Vec2 x1 y1) - (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
  abs :: Vec2 -> Vec2
  abs (Vec2 x y) = Vec2 (abs x) (abs y)

scaleVec2 :: Vec2 -> Float -> Vec2
scaleVec2 (Vec2 x y) scalar = Vec2 (x * scalar) (y * scalar)

dirToVec2 :: Direction -> Vec2
dirToVec2 North = Vec2 0 1
dirToVec2 South = Vec2 0 (-1)
dirToVec2 West = Vec2 (-1) 0
dirToVec2 East = Vec2 1 0

data CellType
  = Invalid
  | Empty
  | Pellet
  | PowerUp
  | Wall
  | Intersection
  | Spawn
  | GhostSpawn GhostType
  | GhostWall
  deriving (Eq)

instance Show CellType where
  show :: CellType -> String
  show Empty = "E"
  show Pellet = "F"
  show Wall = "W"
  show Intersection = "X"
  show Spawn = "S"
  show PowerUp = "A"
  show (GhostSpawn Blinky) = "B"
  show (GhostSpawn Pinky) = "P"
  show (GhostSpawn Inky) = "I"
  show (GhostSpawn Clyde) = "C"
  show GhostWall = "G"
  show Invalid = ""

stringToCellType :: String -> CellType
stringToCellType "F" = Pellet
stringToCellType "W" = Wall
stringToCellType "X" = Intersection
stringToCellType "S" = Spawn
stringToCellType "A" = PowerUp
stringToCellType "B" = GhostSpawn Blinky
stringToCellType "P" = GhostSpawn Pinky
stringToCellType "I" = GhostSpawn Inky
stringToCellType "C" = GhostSpawn Clyde
stringToCellType "G" = GhostWall
stringToCellType "E" = Empty
stringToCellType _ = Invalid

data Cell =
  Cell CellType Vec2

dummyCell :: Cell
dummyCell = Cell Empty (Vec2 0 0)

cellsWithType :: CellType -> [Cell] -> [Cell]
cellsWithType ct = filter (cellHasType ct)

cellHasType :: CellType -> Cell -> Bool
cellHasType ct (Cell t _) = t == ct

-- check if cell has any of the provided types
cellHasTypes :: [CellType] -> Cell -> Bool
cellHasTypes ts (Cell t _) = t `elem` ts

instance Eq Cell where
  (==) :: Cell -> Cell -> Bool
  (Cell t1 v1) == (Cell t2 v2) = t1 == t2 && v1 == v2

instance Show Cell where
  show :: Cell -> String
  show (Cell t _) = show t

data GhostType
  = Blinky
  | Pinky
  | Inky
  | Clyde
  deriving (Eq, Show)

ghosts :: [GhostType]
ghosts = [Blinky, Pinky, Inky, Clyde]

data LevelMap =
  LevelMap Float Float [[Cell]]

instance Show LevelMap where
  show :: LevelMap -> String
  show m@(LevelMap w h l) = intercalate "\n" $ reverse $ map (unwords . map show) l

setCell :: LevelMap -> Cell -> LevelMap
setCell (LevelMap w h cells) c@(Cell _ (Vec2 x y)) = LevelMap w h newCells
  where
    (rs1, rs2) = splitAt (round y) cells
    row = head rs2
    (cs1, cs2) = splitAt (round x) row
    newRow = cs1 ++ (c : tail cs2)
    newCells = rs1 ++ (newRow : tail rs2)

clearCell :: LevelMap -> Vec2 -> LevelMap
clearCell m v = setCell m (Cell Empty v)

setCells :: LevelMap -> [Cell] -> LevelMap
setCells = foldl setCell

getWall :: LevelMap -> Vec2 -> Maybe Cell
getWall m v
  | Just c@(Cell Wall _) <- getCell m v = Just c
  | otherwise = Nothing

getCellWithType :: CellType -> LevelMap -> Vec2 -> Maybe Cell
getCellWithType ct m v
  | Just c@(Cell ct _) <- getCell m v = Just c
  | otherwise = Nothing

getCellCond :: (Cell -> Bool) -> LevelMap -> Vec2 -> Maybe Cell
getCellCond f m v
  | Just c <- getCell m v
  , f c = Just c
  | otherwise = Nothing

getCellsCond :: (Cell -> Bool) -> LevelMap -> [Cell]
getCellsCond f (LevelMap _ _ cells) = filter f $ concat cells

getCellsWithType :: CellType -> LevelMap -> [Cell]
getCellsWithType ct = getCellsCond (cellHasType ct)

getCellsWithTypes :: [CellType] -> LevelMap -> [Cell]
getCellsWithTypes ts = getCellsCond (cellHasTypes ts)

isCellCond :: LevelMap -> (Cell -> Bool) -> Vec2 -> Bool
isCellCond m f v
  | Just c <- getCell m v, f c = True
  | otherwise = False

-- filter list of vectors with conditional on potential Cell and returns filtered list of vec2s
filterLevelVec2s :: LevelMap -> (Cell -> Bool) -> [Vec2] -> [Vec2]
filterLevelVec2s m f = filter (isCellCond m f)

-- filter list of vectors with conditional on potential Cell and returns filtered containing cells matching conditional
filterLevelCells :: LevelMap -> (Cell -> Bool) -> [Vec2] -> [Cell]
filterLevelCells m f = mapMaybe (getCellCond f m)

isOutOfBounds :: LevelMap -> Vec2 -> Bool
isOutOfBounds (LevelMap w h _) (Vec2 x y) = x < 0 || y < 0 || x >= w || y >= h

getCell :: LevelMap -> Vec2 -> Maybe Cell
getCell l@(LevelMap _ _ cells) v@(Vec2 x y)
  | isOutOfBounds l v = Nothing
  | otherwise = Just $ (cells !! round y) !! round x

getCellType :: LevelMap -> Vec2 -> CellType
getCellType m v
  | Just (Cell t _) <- getCell m v = t
  | otherwise = Invalid

getCells :: LevelMap -> [Vec2] -> [Cell]
getCells l = mapMaybe (getCell l)

mapHeight :: [[Cell]] -> Float
mapHeight cs = foldr (max . (\(Cell _ (Vec2 _ y)) -> y)) 0 (concat cs)

mapWidth :: [[Cell]] -> Float
mapWidth cs = foldr (max . (\(Cell _ (Vec2 x _)) -> x)) 0 (concat cs)

parseLevel :: String -> LevelMap
parseLevel s = LevelMap (mapWidth cells + 1) (mapHeight cells + 1) cells
  where
    cells = parseAll (map words (reverse (lines s)))
    parseAll :: [[String]] -> [[Cell]]
    parseAll = zipWith (\y row -> parseRow row (fromInteger y :: Float)) [0 ..]
    parseRow :: [String] -> Float -> [Cell]
    parseRow r y = zipWith (\x t -> Cell (stringToCellType t) (Vec2 (fromInteger x :: Float) y)) [0 ..] r

readLevel :: String -> IO LevelMap
readLevel f = do
  mazeText <- readFile f
  return (parseLevel mazeText)

data GameLevel =
  DefaultLevel -- would only change if we decide to implement additional levels

data PowerUp
  = Cherry
  | Apple
  | PowerPellet
  deriving (Eq)

data Player = Player
  { pVelocity :: Float
  , pDirection :: Direction
  , pLocation :: Point -- point on screen
  , pFrame :: Int
  , pBufferedInput :: Maybe Direction
  , pMoving :: Bool
  } deriving (Generic, Show)

data GhostBehaviour
  = Scatter
  | Chase
  | Idling
  | Respawning
  | Frightened
  deriving (Eq, Show)

data GhostActor = GhostActor
  { ghostType :: GhostType
  , gUpdate :: Float
  , gRespawnTimer :: Float
  , gVelocity :: Float
  , gDirection :: Direction
  , gLocation :: Point
  , gTarget :: Vec2
  , lastDirChange :: Vec2
  , gModeClock :: Float
  , gFrightenedClock :: Float -- TODO: merge frightened clock and respawn timer into one field: "ModeTimer"
  , gCurrentBehaviour :: GhostBehaviour
  , lastModeChange :: Float
  } deriving (Generic, Show)
