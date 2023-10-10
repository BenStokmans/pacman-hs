module Main where
import Pathfinding (getShortestPath)
import Types ( Vec2(Vec2), Cell(Cell), CellType (Intersection), readLevel, setCells )
import Map ( calculateIntersections, calculateWallGroups )
import Text.Printf

start :: Vec2
start = Vec2 1 1
end :: Vec2
end = Vec2 21.0 9.0

main :: IO ()
main = do
  level <- readLevel "level.txt"
  -- printf (show (setCells level (maybe [] (map (Cell Intersection)) (getShortestPath level start end))))
  printf (show (calculateWallGroups level))