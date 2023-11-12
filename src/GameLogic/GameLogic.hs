module GameLogic.GameLogic where

import GameLogic.MapLogic (Direction(North, South), GridInfo, cellSize, getSpawnPoint, gridToScreenPos)
import Rendering (calcSprite16Size, calcSprite32Size)
import State
  ( GameState(fruitEaten, gMap, level, pelletCount, player,
          totalPelletCount)
  , GhostActor(..)
  , GlobalState(gameState, settings)
  , Player(..)
  , Settings(collisionLeniency, fruitPadding, ghostPadding,
         globalSpeedScalar, mazeMargin, pacmanPadding)
  , gameGridInfo
  )

getFruitScore :: GlobalState -> Int
getFruitScore s
  | l == 1 = 100
  | l == 2 = 300
  | l == 3 || l == 4 = 500
  | l == 5 || l == 6 = 700
  | l == 7 || l == 7 = 1000
  | l == 9 || l == 10 = 2000
  | l == 11 || l == 12 = 3000
  | otherwise = 5000
  where
    l = level $ gameState s

speedConstant :: Float -- pps (pixels per second), source: original pacman
speedConstant = 75.75757625 -- On grid with 8x8 pixel cells.

calculateGameSpeed :: GlobalState -> Direction -> Float -> Float -- Directional speed calculation.
calculateGameSpeed gs dir speed = speed * speedScalar * speedConstant * globalSpeedScalar (settings gs)
  where
    gi = gameGridInfo gs
    (cw, ch) = cellSize gi
    speedScalar
      | dir == North || dir == South = ch / 8 --Scalar that scales speedConstant with our cell size in pixels.
      | otherwise = cw / 8

calcGhostSize :: GlobalState -> GridInfo -> (Float, Float)
calcGhostSize gs gi = calcSprite16Size gi ((1 + mazeMargin (settings gs) * 2) * (1 - ghostPadding (settings gs) * 2))

calcPlayerSize :: GlobalState -> GridInfo -> (Float, Float)
calcPlayerSize gs gi = calcSprite16Size gi ((1 + mazeMargin (settings gs) * 2) * (1 - pacmanPadding (settings gs) * 2))

fruitAvailable :: GlobalState -> Bool
fruitAvailable s =
  let gs = gameState s
   in not (fruitEaten gs || (fromIntegral (pelletCount gs) :: Float) < (fromIntegral (totalPelletCount gs) :: Float) / 2)

--Leniency changes the ghost hitbox slightly to make the game more forgiving.
ghostPlayerCollision :: GlobalState -> GridInfo -> GhostActor -> Bool
ghostPlayerCollision gs gi ga
  | abs (px - gx) <= (pw / 2 + gw / 2) * (1 - leniency) && abs (py - gy) <= (ph / 2 + gh / 2) * (1 - leniency) = True
  | otherwise = False
  where
    leniency = collisionLeniency $ settings gs
    (gw, gh) = calcGhostSize gs gi
    (pw, ph) = calcPlayerSize gs gi
    (gx, gy) = gLocation ga
    (px, py) = pLocation $ player $ gameState gs

fruitPlayerCollision :: GlobalState -> GridInfo -> Bool
fruitPlayerCollision s gi
  | not $ fruitAvailable s = False
  | abs (px - fx) <= pw / 2 + fw / 2 && abs (py - fy) <= ph / 2 + fh / 2 = True
  | otherwise = False
  where
    gs = gameState s
    (fw, fh) = calcSprite32Size gi (1 - fruitPadding (settings s))
    (pw, ph) = calcPlayerSize s gi
    (fx, fy) = gridToScreenPos gi $ getSpawnPoint $ gMap gs
    (px, py) = pLocation $ player $ gameState s
