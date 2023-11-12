{-# LANGUAGE BlockArguments #-}

module Views.GameView where

import Assets (Anim(..), Assets(..), PacManSprite(..))
import Control.Monad.Random
import Data.Foldable (foldrM)
import Data.List (delete, minimumBy)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import FontContainer (FontContainer(..))
import GHC.Base (undefined)
import Graphics.Gloss (Color, Picture(Color, Line), Point, blank, blue, circleSolid, green, makeColor, orange, pictures, red, scale, translate, white, rectangleWire, black)
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(..), SpecialKey(..))
import Map
  ( calcNextGhostPosition
  , calcNextPlayerPosition
  , calcWrappedPosition
  , deleteMultiple
  , getGhostSpawnPoint
  , isPastCentre
  , processWalls
  , wallSectionToPic
  , wallToSizedSection, getAllowedGhostDirections, getSpawnPoint
  )
import Pathfinding ( getDirectionsLimited, getPathLimited, getTraveledDirection )
import Rendering (cellSize, drawGrid, gridToScreenPos, renderStringTopLeft, renderStringTopRight, screenToGridPos, translateCell, resize, renderString)
import State (GameState(..), GlobalState(..), MenuRoute(..), Settings(..), getGhostActor, ghostToSprite, gridSizePx, gameGridInfo, ghostActors, DebugSettings (..), Prompt (..), defaultPrompt)
import Struct
  ( Cell(..)
  , CellType(..)
  , Direction(..)
  , GhostActor(..)
  , GhostBehaviour(..)
  , GhostType(..)
  , GridInfo
  , LevelMap(..)
  , Player(..)
  , Vec2(..)
  , allDirections
  , cellHasType
  , cellsWithType
  , clearCell
  , dirToVec2
  , dummyCell
  , getCell
  , getCellCond
  , getCellType
  , ghosts
  , isCellCond
  , isOutOfBounds
  , oppositeDirection
  , outOfBounds
  , scaleVec2
  , setCell, getCellsWithType, cellHasTypes, setCells, adjacentVecs, filterLevelVec2s, headMaybe
  )
import GhostLogic ( updateGhostTarget, updateGhostClock, setGhostBehaviour, updateGhostGlobalState, updateGhostVelocity, updateGhostGameState, hasFrightenedGhost, inWarpTunnel )
import Data.Aeson (encode)
import Data.Map (insert)

debugGrid :: GlobalState -> Picture
debugGrid s = drawGrid (gameGridInfo s) green

pelletColor :: Color
pelletColor = makeColor 0.96 0.73 0.61 1

getGhostColor :: GhostType -> Color
getGhostColor Blinky = red
getGhostColor Pinky = makeColor 1 0.72 1 1
getGhostColor Inky = makeColor 0 1 1 1
getGhostColor Clyde = makeColor 1 0.72 0.32 1

getFruit :: GlobalState -> Picture
getFruit s | l == 1             = cherrySprite a
           | l == 2             = strawBerrySprite a
           | l == 3  || l == 4  = melonSprite a
           | l == 5  || l == 6  = appleSprite a
           | l == 7  || l == 7  = melonSprite a
           | l == 9  || l == 10 = galaxianSprite a
           | l == 11 || l == 12 = bellSprite a
           | otherwise          = keySprite a
  where
    l = level $ gameState s
    a = assets s

getFruitScore :: GlobalState -> Int
getFruitScore s | l == 1             = 100
                | l == 2             = 300
                | l == 3  || l == 4  = 500
                | l == 5  || l == 6  = 700
                | l == 7  || l == 7  = 1000
                | l == 9  || l == 10 = 2000
                | l == 11 || l == 12 = 3000
                | otherwise          = 5000
  where
    l = level $ gameState s

speedConstant :: Float -- pps (pixels per second), source: original pacman
speedConstant = 75.75757625 -- on grid with 8x8 pixel cells

calculateGameSpeed :: GlobalState -> Direction -> Float -> Float -- directional speed calculation
calculateGameSpeed gs dir speed = speed * speedScalar * speedConstant * globalSpeedScalar (settings gs)
  where
    gi = gameGridInfo gs
    (cw,ch) = cellSize gi
    speedScalar | dir == North || dir == South = ch/8
                | otherwise = cw/8

debugGhostPath :: GlobalState -> Picture
debugGhostPath s =
  pictures $
  map
    (\g ->
       Color (getGhostColor g) $
       pictures $
       maybe
         []
         (map
            (\v ->
               let (x, y) = gridToScreenPos dims v
                in translate x y $ scale 1 (ch / cw) $ circleSolid (cw / 3))) $
       getPathLimited
         (gMap gs)
         (oppositeDirection $ gDirection $ getGhostActor s g)
         (screenToGridPos dims (gLocation $ getGhostActor s g))
         (gTarget $ getGhostActor s g)
         True)
    ghosts
  where
    gs = gameState s
    dims@((c, r), (w, h)) = gameGridInfo s
    (cw, ch) = cellSize dims

debugGhostTargets :: GlobalState -> Picture
debugGhostTargets s =
  pictures $
  map
    (\gt ->
       Color (getGhostColor gt) $
       let (x, y) = gridToScreenPos dims (gTarget $ getGhostActor s gt)
        in translate x y $ scale 1 (ch / cw) $ circleSolid (cw / 3))
    ghosts
  where
    dims@((c, r), (w, h)) = gameGridInfo s
    (cw, ch) = cellSize dims

drawMap :: GlobalState -> LevelMap -> GridInfo -> Picture
drawMap gs m@(LevelMap _ _ cells) gi@((col, row), (w, h)) =
  Color blue $
  pictures $
  map (\(c, ws) -> Color blue $ translateCell c gi (wallToSizedSection margin t cw ch ws)) (filter (cellHasType Wall . fst) $ cachedWalls gs) ++
  map (\(c, ws) -> Color orange $ translateCell c gi (wallToSizedSection margin t cw ch ws)) (filter (cellHasType GhostWall . fst) $ cachedWalls gs) ++
  map (\c -> Color pelletColor $ translateCell c gi $ scale 1 (ch / cw) $ circleSolid (cw / 12)) (getCellsWithType Pellet m) ++
  map (\c -> Color pelletColor $ translateCell c gi $ scale 1 (ch / cw) $ circleSolid (cw / 3)) (getCellsWithType PowerUp m)
  where
    ass = assets gs
    margin = mazeMargin $ settings gs
    t = lineThickness $ settings gs
    (w2, h2) = (w / 2, h / 2)
    (cw, ch) = cellSize gi

getPlayerAnimation :: GlobalState -> Anim
getPlayerAnimation gs
  | d == South = down as
  | d == West = left as
  | d == East = right as
  | otherwise = up as
  where
    d = pDirection $ player $ gameState gs
    as = pacSprite $ assets gs

calcSpriteSize :: GridInfo -> (Float, Float) -> Float -> (Float, Float)
calcSpriteSize gi@((c, r), _) (w, h) scalar = let (cw, ch) = cellSize gi in (w * (cw / w * scalar * (c / r)), h * (ch / h * scalar * (r / c)))

calcSprite16Size :: GridInfo -> Float -> (Float, Float)
calcSprite16Size gi = calcSpriteSize gi (16,16)

calcSprite32Size :: GridInfo -> Float -> (Float, Float)
calcSprite32Size gi = calcSpriteSize gi (32,32)

calcGhostSize :: GlobalState -> GridInfo -> (Float,Float)
calcGhostSize gs gi = calcSprite16Size gi ((1 + mazeMargin (settings gs) * 2) * (1 - ghostPadding (settings gs) * 2))

calcPlayerSize :: GlobalState -> GridInfo -> (Float,Float)
calcPlayerSize gs gi = calcSprite16Size gi ((1 + mazeMargin (settings gs) * 2) * (1 - pacmanPadding (settings gs) * 2))

drawGhost :: GlobalState -> GhostActor -> GridInfo -> Point -> Picture
drawGhost gs ghost gi (px, py) | ghostM == Respawning = translate px py $ scale (timer/respawnLength) (timer/respawnLength) sprite
                               | otherwise = translate px py sprite
    where
      ghostM = gCurrentBehaviour ghost
      (w,h) = calcGhostSize gs gi
      sprite = resize 16 16 w h (ghostToSprite gs ghost)
      timer = gRespawnTimer ghost
      respawnLength = ghostRespawnTimer $ settings gs

fruitAvailable :: GlobalState -> Bool
fruitAvailable s = let gs = gameState s in not (fruitEaten gs || (fromIntegral (pelletCount gs) :: Float) < (fromIntegral (totalPelletCount gs) :: Float) / 2)

drawFruit :: GlobalState -> GridInfo -> Picture
drawFruit s gi | fruitAvailable s = translate x y $ resize 32 32 w h $ getFruit s
               | otherwise = blank
  where
    gs = gameState s
    (x, y) = gridToScreenPos gi $ getSpawnPoint $ gMap gs
    (w,h) = calcSprite32Size gi (1 - fruitPadding (settings s))

drawPlayer :: GlobalState -> GridInfo -> Point -> Picture
drawPlayer gs gi (px, py) = let (w,h) = calcPlayerSize gs gi in translate px py $ resize 16 16 w h (getPlayerAnimation gs !! pFrame (player $ gameState gs))

ghostPlayerCollision :: GlobalState -> GridInfo -> GhostActor -> Bool
ghostPlayerCollision gs gi ga | abs (px - gx) <= (pw/2 + gw/2)*(1-leniency) && abs (py - gy) <= (ph/2 + gh/2)*(1-leniency) = True
                              | otherwise = False
  where
    leniency = collisionLeniency $ settings gs
    (gw,gh) = calcGhostSize gs gi
    (pw,ph) = calcPlayerSize gs gi
    (gx,gy) = gLocation ga
    (px,py) = pLocation $ player $ gameState gs

fruitPlayerCollision :: GlobalState -> GridInfo -> Bool
fruitPlayerCollision s gi | not $ fruitAvailable s = False
                          | abs (px - fx) <= pw/2 + fw/2 && abs (py - fy) <= ph/2 + fh/2 = True
                          | otherwise = False
  where
    gs = gameState s
    (fw,fh) = calcSprite32Size gi (1 - fruitPadding (settings s))
    (pw,ph) = calcPlayerSize s gi
    (fx,fy) = gridToScreenPos gi $ getSpawnPoint $ gMap gs
    (px,py) = pLocation $ player $ gameState s

getGhostDebugString :: GlobalState -> GhostType -> String
getGhostDebugString gs gt = show (screenToGridPos gi $ gLocation ghost) ++
       ", " ++
       show (gTarget ghost) ++
       ", " ++
       show (gDirection ghost) ++
        ", " ++
       show (getAllowedGhostDirections (gMap $ gameState gs) (gDirection ghost) (screenToGridPos gi $ gLocation ghost))  ++
       ", " ++
       show (gVelocity ghost) ++
       ", " ++
       show (inWarpTunnel gs ghost)
       ++ "\n"
       where
        gi = gameGridInfo gs
        ghost = getGhostActor gs gt

drawBoundingBox :: (Float,Float) -> (Float,Float) -> Picture
drawBoundingBox (x,y) (w,h) = Color white $ translate x y $ rectangleWire w h

drawGhostsBoundingBox :: GlobalState -> Picture
drawGhostsBoundingBox gs = Color white $ pictures $ map (\g -> let (gx,gy) = gLocation g in translate gx gy $ rectangleWire gw gh) $ ghostActors gs
    where (gw,gh) = calcGhostSize gs (gameGridInfo gs)

drawPlayerBoundingBox :: GlobalState -> Picture
drawPlayerBoundingBox gs = drawBoundingBox (pLocation $ player $ gameState gs) $ calcPlayerSize gs (gameGridInfo gs)

drawFruitBoundingBox :: GlobalState -> Picture
drawFruitBoundingBox s | fruitAvailable s = drawBoundingBox (gridToScreenPos gi $ getSpawnPoint $ gMap gs) $ calcSprite32Size gi (1 - fruitPadding (settings s))
                       | otherwise = blank
  where
    gi = gameGridInfo s
    gs = gameState s

getDebugPicture :: GlobalState -> IO Picture
getDebugPicture s = do
  let sett = settings s
  let gs = gameState s
  let emuS = FontContainer.s (emuFont (assets s))
  ghostString <- renderStringTopRight
    (400, 400)
    emuS
    green
    ("Maze margin: " ++
      show (mazeMargin sett) ++
      ", Pac-Man padding: " ++
      show (pacmanPadding sett) ++
      "\nBlinky: " ++ getGhostDebugString s Blinky ++
      "Inky: " ++ getGhostDebugString s Inky ++
      "Pinky: " ++ getGhostDebugString s Pinky ++
      "Clyde: " ++ getGhostDebugString s Clyde
      )
  gameString <- renderStringTopRight
    (400, 350)
    emuS
    green
    ("FPS: " ++ show (round $ 1/lastClock s) ++ 
     "\nGod: " ++ show (godMode gs) ++
     "\nLevel: " ++ show (level gs) ++
     "\nFruit: " ++ show (fruitAvailable s) ++
     "\nPellets: " ++ show (pelletCount gs) ++
     "\nTotal: " ++ show (totalPelletCount gs) ++
     "\nFruitP: " ++ show (round ((fromIntegral (pelletCount gs) :: Float) / ((fromIntegral (totalPelletCount gs) :: Float) / 2) * 100)) ++ "%" ++
     "\nLevelP: " ++ show (round ((fromIntegral (pelletCount gs) :: Float) / (fromIntegral (totalPelletCount gs) :: Float) * 100)) ++ "%"
    )
  let dSett = debugSettings sett
  let boundingBoxes = if enableHitboxes dSett then [drawPlayerBoundingBox s, drawGhostsBoundingBox s, drawFruitBoundingBox s] else []
  let debugs = [
          [debugGrid s | enableGrid dSett],
          [debugGhostPath s | enableGhostPath dSett],
          [ghostString | enableGhostText dSett],
          [debugGhostTargets s | enableGhostTarget dSett],
          boundingBoxes,
          [gameString | enableGameText dSett]
          ]
  return $ if debugEnabled sett then pictures $ concat debugs else blank

renderGameView :: GlobalState -> IO Picture
renderGameView s = do
  let gs = gameState s
  let currentLevel = gMap gs
  let gi = gameGridInfo s
  scoreString <- renderStringTopLeft (-400, 400) (FontContainer.m (emuFont (assets s))) white $ "Score: " ++ show (score gs)
  let drawnMap = drawMap s currentLevel gi
  let drawnGhosts = pictures $ map (\t -> let ghost = getGhostActor s t in drawGhost s ghost gi $ gLocation ghost) [Blinky, Pinky, Inky, Clyde]
  let drawnLives = pictures $ map (\v -> translate ((- 375) + 40 * (fromIntegral v :: Float)) (- 375) $ scale 2 2 $ head (right $ pacSprite $ assets s)) [0..lives gs-1]
  let drawnLevelFruit = translate 375 (-375) $ getFruit s
  let pl = pLocation $ player gs
  gameOverString <- if lives gs == 0 then renderString (0,0) (l (emuFont (assets s))) red "GAME OVER" else do return blank
  drawnKillText <- if pauseGameTimer gs > 0 && killingSpree gs > 0 then renderStringTopLeft pl (m (emuFont (assets s))) white $ show (200*2^(killingSpree gs - 2)) else return blank
  debug <- getDebugPicture s
  return
    (pictures
       [ drawnMap
       , drawnLives
       , drawPlayer s gi pl
       , drawnGhosts
       , drawFruit s gi
       , scoreString
       , debug
       , drawnKillText
       , drawnLevelFruit
       , gameOverString
       ])

keyToDirection :: Direction -> Key -> Direction
keyToDirection _ (SpecialKey KeyUp) = North
keyToDirection _ (SpecialKey KeyDown) = South
keyToDirection _ (SpecialKey KeyLeft) = West
keyToDirection _ (SpecialKey KeyRight) = East
keyToDirection _ (Char 'w') = North
keyToDirection _ (Char 'a') = West
keyToDirection _ (Char 's') = South
keyToDirection _ (Char 'd') = East
keyToDirection d _ = d

handleInputGameView :: Event -> GlobalState -> IO GlobalState
handleInputGameView (EventKey (SpecialKey KeyEsc) _ _ _) gs = return gs {route = PauseMenu, history = [GameView]}
handleInputGameView (EventKey (Char 'g') _ _ _) s = return s {gameState = (gameState s) { godMode = not (godMode $ gameState s) }}
handleInputGameView (EventKey (Char 'l') _ _ _) s = return s {gameState = (gameState s) { level = level (gameState s) + 1}}

handleInputGameView (EventKey k _ _ _) s = return s {gameState = gs {player = ps {pBufferedInput = bufferedInput, pDirection = direction}}}
  where
    gs = gameState s
    ps = player gs
    newDir = keyToDirection oldDir k
    oldDir = pDirection ps
    bufferedInput
      | newDir == oldDir || newDir == oppositeDirection oldDir = pBufferedInput ps
      | otherwise = Just newDir
    direction
      | newDir /= oldDir && newDir /= oppositeDirection oldDir = oldDir
      | otherwise = newDir -- technically not proper but it works
handleInputGameView _ s = return s

saveGameState :: GlobalState -> IO ()
saveGameState s = do writeFile "assets/highscores.json" (show (encode $ highScores s))

confirmHighScorePrompt :: GlobalState -> String -> IO GlobalState
confirmHighScorePrompt s v
  | v /= "" = do
    let newState = s {highScores = insert v (score $ gameState s) (highScores s)}
    writeFile "assets/highscores.json" (show (encode $ highScores newState))
    return newState {route = StartMenu, prompt = Nothing}
  | otherwise = do return s {route = StartMenu, prompt = Nothing}
  where
    set = settings s


updateAnimationClocks :: GlobalState -> Float -> GlobalState
updateAnimationClocks s d | pauseGameTimer gs <= 0 && lives gs == 0 = s
              { prompt =
                  Just
                    defaultPrompt
                      { promptText = "Enter your name: "
                      , promptValue = "name"
                      , confirmAction = confirmHighScorePrompt
                      , closeAction = \state _ -> do return state {route = StartMenu, prompt = Nothing}
                      },
                gameState = gs { pauseGameTimer = 999}
              }
              | otherwise = s { gameState = gs { pauseGameTimer = pauseGameTimer gs - d } }
  where
    gs = gameState s

updatePlayerAnimState :: GlobalState -> GlobalState
updatePlayerAnimState s
  | not $ pMoving ps = s {gameState = gs {prevClock = p + (c - p)}}
  | c - p >= animFrameCooldown = s
        { gameState =
            gs
              { player =
                  ps
                    { pFrame =
                        if fr == length anim - 1
                          then 0
                          else fr + 1
                    }
              , prevClock = c
              }
        }
  | otherwise = s
  where
    animFrameCooldown = 8/calculateGameSpeed s (pDirection ps) (pVelocity ps)
    gs = gameState s
    ps = player gs
    anim = getPlayerAnimation s
    fr = pFrame ps
    c = clock s
    p = prevClock gs

updateGhostPosition :: Float -> GlobalState -> GhostActor -> GlobalState
updateGhostPosition dt s ghost = s {gameState = newGameState}
  where
    gs = gameState s
    dims@((c, r), (w, h)) = gameGridInfo s
    (wc, hc) = cellSize dims
    m@(LevelMap lw lh cells) = gMap gs
    currentDirection = gDirection ghost
    location = gLocation ghost
    currentGridPos = screenToGridPos dims location
    distMoved = calculateGameSpeed s currentDirection (gVelocity ghost) * dt
    nextCellType = getCellType m (currentGridPos + dirToVec2 currentDirection)
    wrappedPos = calcWrappedPosition dims currentDirection location
    pastCenter = isPastCentre dims currentDirection currentGridPos wrappedPos
    newLoc@(nx, ny) = calcNextGhostPosition currentDirection nextCellType pastCenter wrappedPos distMoved
    (cx, cy) = gridToScreenPos dims currentGridPos
    Cell ctype cLoc = fromMaybe dummyCell (getCell m currentGridPos) -- it is assumed that it is not nothing
    walls = cellsWithType
           Wall
           (mapMaybe
              (\d -> getCell m (currentGridPos + dirToVec2 d))
              (deleteMultiple allDirections [oppositeDirection currentDirection, currentDirection]))
    path = fromMaybe [currentDirection] $ getDirectionsLimited m (oppositeDirection currentDirection) currentGridPos (gTarget ghost) True
    allowedDirections = getAllowedGhostDirections m currentDirection currentGridPos

    oldChange = lastDirChange ghost
    (newDir, newChange)
      -- | pastCenter && not (null allowedDirections)
      | oldChange == currentGridPos = (currentDirection, oldChange)
      | gTarget ghost == currentGridPos && not (null allowedDirections) = (head allowedDirections, currentGridPos)
      | gTarget ghost == currentGridPos && null allowedDirections && isOutOfBounds m (currentGridPos + dirToVec2 currentDirection) = (currentDirection, oldChange)
      | gTarget ghost == currentGridPos && null allowedDirections = (oppositeDirection currentDirection, currentGridPos) -- this doesn't work exactly like I want it to
      | null path = (currentDirection, oldChange)
      | pastCenter && length walls < 2 = (head path, currentGridPos)
      | otherwise = (currentDirection, oldChange)
    pastCentreLocation
      | newDir == North || newDir == South = (cx, ny)
      | otherwise = (nx, cy)
    finalLocation
      | currentDirection /= newDir = pastCentreLocation
      | otherwise = newLoc
    newGhost = ghost {gLocation = finalLocation, gDirection = newDir, lastDirChange = newChange, gUpdate = if finalLocation /= location then 0 else gUpdate ghost}
    newGameState = updateGhostGameState gs newGhost

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

checkCollisionsForGhost :: GlobalState -> GhostActor -> GlobalState
checkCollisionsForGhost s ghost | godMode gs = s
                                | colliding && gCurrentBehaviour ghost == Frightened = deadGhostGS { gameState = (gameState deadGhostGS) { score = score gs + 200*(2^(ks-1)), killingSpree = ks+1, pauseGameTimer = 1 } }
                                | colliding && gCurrentBehaviour ghost == Respawning = s
                                | colliding = foldr (\g ts -> updateGhostGlobalState ts (getGhostActor ts g) {gLocation = gridToScreenPos gi $ getGhostSpawnPoint level g}) deadPlayerGS ghosts
                                | otherwise = s
                                where
                                  gi = gameGridInfo s
                                  colliding = ghostPlayerCollision s gi ghost
                                  gs = gameState s
                                  level = gMap gs
                                  ks = killingSpree gs
                                  ghostT = ghostType ghost

                                  spawnPoint = getGhostSpawnPoint level ghostT
                                  respawnPos = gridToScreenPos gi $ getGhostSpawnPoint level ghostT
                                  allowedDirections = filter (\d -> isCellCond level (not . cellHasType Wall) (spawnPoint + dirToVec2 d)) allDirections ++ [gDirection ghost] -- the addition of the current direction is purely a failsafe, this could only happen if a map maker decides to put the ghost in a box
                                  respawnGhost = ghost { gLocation = gridToScreenPos gi $ getGhostSpawnPoint level ghostT, gCurrentBehaviour = Respawning, gFrightenedClock = 0, gDirection = head allowedDirections, lastDirChange = spawnPoint }
                                  deadGhostGS = updateGhostGlobalState s respawnGhost

                                  deadPlayerGS | lives gs == 1 = s { gameState = gs { lives = 0, pauseGameTimer = 2, player = (player gs) { pLocation = (-1000,-1000)}} } -- properly handle game over
                                               | otherwise = s { gameState = gs {lives = lives gs - 1, killingSpree = 0, pauseGameTimer = 1, player = (player gs) { pLocation = gridToScreenPos gi $ getSpawnPoint level, pDirection = fromMaybe North $ headMaybe $ map (getTraveledDirection (getSpawnPoint level)) $ filterLevelVec2s level (not . cellHasTypes [Wall,GhostWall]) $ adjacentVecs (getSpawnPoint level)}}}

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

handleUpdateGameView :: Float -> GlobalState -> IO GlobalState
handleUpdateGameView f gs = do
  let updatedAnimationClocks = updateAnimationClocks gs f
  let updatedClocks = foldr (\g acc ->updateGhostClock acc f (getGhostActor gs g)) updatedAnimationClocks ghosts
  let ngs = updatePlayerAnimState updatedClocks
  let updatedPlayerVelocity = updatePlayerVelocity ngs
  let pUpdate = updatePlayerPosition f updatedPlayerVelocity
  -- update ghosts target
  ghostTargetUpdate <- foldrM (\v acc -> updateGhostTarget (getGhostActor acc v) acc) pUpdate ghosts
  -- update ghosts velocity
  let ghostVelocityUpdate = foldr (\v acc -> updateGhostVelocity acc (getGhostActor acc v)) ghostTargetUpdate ghosts
  -- update ghosts position
  let ghostPositionUpdate = foldr (\v acc -> updateGhostPosition f acc (getGhostActor acc v)) ghostVelocityUpdate ghosts
  -- check for ghosts collision with the player
  let collisionUpdate = foldr (\v acc -> checkCollisionsForGhost acc (getGhostActor acc v)) ghostPositionUpdate ghosts
  return $ if pauseGameTimer (gameState updatedAnimationClocks) > 0 then updatedAnimationClocks else collisionUpdate