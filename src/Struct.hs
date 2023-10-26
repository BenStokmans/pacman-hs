module Struct where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Graphics.Gloss (Point)

type GridInfo = ((Float, Float), (Float, Float))

data Direction
  = North
  | South
  | West
  | East
  deriving (Eq)

instance Show Direction where
  show :: Direction -> String
  show North = "North"
  show South = "South"
  show West = "West"
  show East = "East"

allDirections :: [Direction]
allDirections = [North, South, West, East]

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

scaleVec2 :: Vec2 -> Float -> Vec2
scaleVec2 (Vec2 x y) scalar = Vec2 (x * scalar) (y * scalar)

dirToVec2 :: Direction -> Vec2
dirToVec2 North = Vec2 0 1
dirToVec2 South = Vec2 0 (-1)
dirToVec2 West = Vec2 (-1) 0
dirToVec2 East = Vec2 1 0

data CellType
  = Empty
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
stringToCellType _ = Empty

data Cell =
  Cell CellType Vec2

dummyCell :: Cell
dummyCell = Cell Empty (Vec2 0 0)

cellsWithType :: CellType -> [Cell] -> [Cell]
cellsWithType ct = filter (cellHasType ct)

cellHasType :: CellType -> Cell -> Bool
cellHasType ct (Cell t _) = t == ct

instance Eq Cell where
  (==) :: Cell -> Cell -> Bool
  (Cell t1 v1) == (Cell t2 v2) = t1 == t2 && v1 == v2

instance Show Cell where
  show :: Cell -> String
  show (Cell t v) = show t ++ ":" ++ show v

data GhostType
  = Blinky
  | Pinky
  | Inky
  | Clyde
  deriving (Eq)

ghosts :: [GhostType]
ghosts = [Blinky, Pinky, Inky, Clyde]

data LevelMap =
  LevelMap Float Float [Cell]

instance Show LevelMap where
  show :: LevelMap -> String
  show m@(LevelMap w h l) = intercalate "\n" $ reverse (map (unwords . reverse) cells)
    where
      indeces = map (\y -> map (`Vec2` y) [0 .. w - 1]) [0 .. h - 1]
      cells = map (map (maybe "E" (\(Cell t _) -> show t) . getCell m)) indeces

setCell :: LevelMap -> Cell -> LevelMap
setCell (LevelMap w h m) c@(Cell _ v1) = LevelMap w h (c : filter (\(Cell _ v2) -> v1 /= v2) m)

setCells :: LevelMap -> [Cell] -> LevelMap
setCells = foldl setCell

getCell :: LevelMap -> Vec2 -> Maybe Cell -- recursion should be way faster than the function below in the best case and the same in the worst case
getCell (LevelMap _ _ []) _ = Nothing
getCell (LevelMap w h (c@(Cell _ v2@(Vec2 x y)):xs)) v1
  | v1 == v2 = Just c
                                                --    | x < 0 || y < 0 || x >= w || y >= h = Just OutOfBounds
  | otherwise = getCell (LevelMap w h xs) v1

getCell' :: LevelMap -> Vec2 -> Maybe Cell
getCell' (LevelMap _ _ m) v1
  | null n = Nothing
  | otherwise = Just (head n)
  where
    n = filter (\(Cell _ v2) -> v1 == v2) m

getCells :: LevelMap -> [Vec2] -> [Cell]
getCells l = mapMaybe (getCell l)

mapHeight :: [Cell] -> Float
mapHeight = foldr (max . (\(Cell _ (Vec2 _ y)) -> y)) 0

mapWidth :: [Cell] -> Float
mapWidth = foldr (max . (\(Cell _ (Vec2 x _)) -> x)) 0

parseLevel :: String -> LevelMap
parseLevel s = LevelMap (mapWidth cells + 1) (mapHeight cells + 1) cells
  where
    cells = parseAll (map words (reverse (lines s)))
    parseAll :: [[String]] -> [Cell]
    parseAll rows = concatMap (\(y, row) -> parseRow row (fromInteger y :: Float)) (zip [0 ..] rows)
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
  }

data GhostBehaviour
  = Scatter
  | Chase
  | Idling
  | Respawning
  | Frightened
  deriving (Eq)

data GhostActor = GhostActor
  { ghost :: GhostType
  , gVelocity :: Float
  , gDirection :: Direction
  , gLocation :: Vec2
  , gTarget :: Vec2
  , gBehaviourTimer :: Int
  , gCurrentBehaviour :: GhostBehaviour
  }
