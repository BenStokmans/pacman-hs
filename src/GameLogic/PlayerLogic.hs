module GameLogic.PlayerLogic where

import State
    ( GlobalState(gameState),
      GameState(player, totalPelletCount, pellets, level, pelletCount,
                fruitEaten, gMap, score, killingSpree),
      gameGridInfo,
      getGhostActor, Player (..), GhostBehaviour (..) )
import GameLogic.MapLogic
import Rendering
import GameLogic.GameLogic
    ( getFruitScore, calculateGameSpeed, fruitPlayerCollision )
import Data.Maybe ( fromMaybe )
import GameLogic.GhostLogic
    ( setGhostBehaviour, hasFrightenedGhost )
import GameLogic.Struct (ghosts)


-- player velocity varies based on whether the ghosts are frightened and on whether pacman is eating pellets.
getPlayerVelocity :: GlobalState -> Float
getPlayerVelocity s | hfg && isOnPellet = frightPacDotSpd
                    | hfg = frightPacSpd
                    | isOnPellet = pacDotSpd
                    | otherwise = pacSpd
  where
    ps = player gs
    gi = gameGridInfo s
    gs = gameState s
    l = level gs
    isOnPellet = isCellCond (gMap gs) (cellHasType Pellet) (screenToGridPos gi (pLocation ps))
    hfg = hasFrightenedGhost s
    frightPacDotSpd | l == 1 = 0.79
                    | l < 5 = 0.83
                    | otherwise = 0.87
    frightPacSpd | l == 1 = 0.9
                 | l < 5 = 0.95
                 | otherwise = 1
    pacDotSpd | l == 1 = 0.71
              | l < 5 = 0.79
              | l < 21 = 0.87
              | otherwise = 0.79
    pacSpd | l == 1 = 0.8
           | l < 5 || l > 20 = 0.9
           | otherwise = 1

updatePlayerVelocity :: GlobalState -> GlobalState
updatePlayerVelocity s = s {gameState = gs{player = ps {pVelocity = getPlayerVelocity s}}}
  where
    gs = gameState s
    ps = player gs

updatePlayerPosition :: Float -> GlobalState -> GlobalState
updatePlayerPosition dt s
  | ctype == PowerUp = foldr (\v acc -> setGhostBehaviour acc (getGhostActor acc v) Frightened) newState ghosts
  | otherwise = newState
  where
    gs = gameState s
    dims@((c, r), (w, h)) = gameGridInfo s
    (wc, hc) = cellSize dims
    m@(LevelMap lw lh cells) = gMap gs
    ps = player gs
    currentDirection = pDirection ps
    location = pLocation ps
    currentGridPos = screenToGridPos dims location
    distMoved = calculateGameSpeed s currentDirection (pVelocity ps) * dt

    nextCellType = getCellType m (currentGridPos + dirToVec2 currentDirection)
    wrappedPos = calcWrappedPosition dims currentDirection location
    pastCenter = isPastCentre dims currentDirection currentGridPos wrappedPos
    newLoc@(nx, ny) = calcNextPlayerPosition currentDirection nextCellType pastCenter wrappedPos distMoved
    (cx, cy) = gridToScreenPos dims currentGridPos
    Cell ctype cLoc = fromMaybe dummyCell (getCell m currentGridPos) -- it is assumed that it is not nothing
    bufferedInput = pBufferedInput ps
    canTurn =
      maybe False (\d -> isCellCond m (not . cellHasTypes [Wall,GhostWall]) (currentGridPos + dirToVec2 d)) bufferedInput
    newDir
      | canTurn = fromMaybe North bufferedInput
      | otherwise = currentDirection
    newDirLocation
      | newDir == North || newDir == South = (cx, ny)
      | otherwise = (nx, cy)
    finalLocation
      | currentDirection /= newDir = newDirLocation
      | otherwise = newLoc
    oldScore = score gs
    oldPelletCount = pelletCount gs
    (newScore, newPelletCount, newMap)
      | ctype == Pellet = (oldScore + 10, oldPelletCount + 1, clearCell m cLoc)
      | ctype == PowerUp =
        ( oldScore + 50
        , oldPelletCount + 1
        , clearCell m cLoc
         )
      | otherwise = (oldScore, oldPelletCount, m)
    respawnPellets = newPelletCount >= totalPelletCount gs
    eatFruit = fruitPlayerCollision s dims
    pelletGameState | newPelletCount >= totalPelletCount gs = gs { pelletCount = 0, gMap = setCells newMap (pellets gs), fruitEaten = False, level = level gs + 1 }
                    | otherwise = gs { pelletCount = newPelletCount, fruitEaten = fruitEaten gs || eatFruit, gMap = newMap }

    newState | pastCenter = s { gameState =
                  pelletGameState
                    { score = if eatFruit then newScore + getFruitScore s else newScore
                    , killingSpree = if ctype == PowerUp then 1 else killingSpree gs
                    , player =
                        ps
                          { pLocation = finalLocation
                          , pDirection = newDir
                          , pBufferedInput =
                              if currentDirection /= newDir
                                then Nothing
                                else bufferedInput
                          , pMoving = finalLocation /= location
                          }
                    }
              }
            | otherwise = s {gameState = gs {player = ps {pLocation = newLoc, pMoving = newLoc /= location}}}