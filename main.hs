module Main where
import Pathfinding (getShortestPath)
import Types ( Vec2(Vec2), Cell(Cell), CellType (Cross), readLevel, setCells )
import Text.Printf

start :: Vec2
start = Vec2 1 1
end :: Vec2
end = Vec2 21.0 9.0

main :: IO ()
main = do
  level <- readLevel "level.txt"
  printf (show (setCells level (maybe [] (map (Cell Cross)) (getShortestPath level start end))))