module Views.GameView where
import State
    ( GlobalState(gameState, cachedWalls, lastClock, assets, history,
                  settings, highScores, route, prompt, clock),
      GameState(pauseGameTimer, pelletCount, totalPelletCount, gMap,
                killingSpree, godMode, level, score, lives, player, prevClock),
      Settings(debugEnabled, lineThickness, ghostRespawnTimer,
               fruitPadding, mazeMargin, pacmanPadding, debugSettings),
      gameGridInfo,
      ghostActors,
      MenuRoute(StartMenu, PauseMenu, GameView, LeaderBoardView),
      DebugSettings(enableGameText, enableHitboxes, enableGrid,
                    enableGhostPath, enableGhostText, enableGhostTarget),
      Prompt(closeAction, promptText, promptValue, confirmAction),
      defaultPrompt,
      ghostToSprite,
      getGhostActor )
import Graphics.Gloss
    ( blue,
      green,
      orange,
      red,
      white,
      blank,
      circleSolid,
      pictures,
      rectangleWire,
      scale,
      translate,
      makeColor,
      Color,
      Picture(Color),
      Point )
import GameLogic.Struct
    ( GhostActor(gLocation, gCurrentBehaviour, gRespawnTimer, gTarget,
                 gDirection, gVelocity),
      GhostBehaviour(Respawning),
      Player(pFrame, pLocation, pBufferedInput, pDirection, pVelocity,
             pMoving),
      LevelMap(..),
      GhostType(..),
      CellType(PowerUp, Wall, GhostWall, Pellet),
      Direction(..),
      GridInfo,
      oppositeDirection,
      cellHasType,
      ghosts,
      getCellsWithType )
import Assets
    ( Assets(emuFont, cherrySprite, strawBerrySprite, appleSprite,
             melonSprite, galaxianSprite, bellSprite, keySprite, pacSprite),
      PacManSprite(right, down, left, up),
      Anim )
import Graphics.Gloss.Interface.IO.Game
    ( Key(Char, SpecialKey),
      Event(EventKey),
      SpecialKey(KeyEsc, KeyUp, KeyDown, KeyLeft, KeyRight) )
import Data.Map (insert)
import GameLogic.GhostLogic
    ( updateGhostTarget,
      updateGhostClock,
      inWarpTunnel,
      updateGhostVelocity,
      updateGhostPosition,
      checkCollisionsForGhost )
import GameLogic.PlayerLogic
    ( updatePlayerVelocity, updatePlayerPosition )
import Data.Foldable (foldrM)
import GameLogic.GameLogic
    ( calculateGameSpeed,
      calcGhostSize,
      calcPlayerSize,
      fruitAvailable )
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO
import Rendering
    ( calcSprite32Size,
      gridToScreenPos,
      drawGrid,
      screenToGridPos,
      translateCell,
      resize,
      renderStringTopRight,
      renderStringTopLeft,
      renderString,
      cellSize )
import FontContainer ( FontContainer(m, s, l) )
import GameLogic.Map
    ( getAllowedGhostDirections, getSpawnPoint, wallToSizedSection )
import GameLogic.Pathfinding ( getPathLimited )

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

drawGhost :: GlobalState -> GhostActor -> GridInfo -> Point -> Picture
drawGhost gs ghost gi (px, py) | ghostM == Respawning = translate px py $ scale (timer/respawnLength) (timer/respawnLength) sprite
                               | otherwise = translate px py sprite
    where
      ghostM = gCurrentBehaviour ghost
      (w,h) = calcGhostSize gs gi
      sprite = resize 16 16 w h (ghostToSprite gs ghost)
      timer = gRespawnTimer ghost
      respawnLength = ghostRespawnTimer $ settings gs

drawFruit :: GlobalState -> GridInfo -> Picture
drawFruit s gi | fruitAvailable s = translate x y $ resize 32 32 w h $ getFruit s
               | otherwise = blank
  where
    gs = gameState s
    (x, y) = gridToScreenPos gi $ getSpawnPoint $ gMap gs
    (w,h) = calcSprite32Size gi (1 - fruitPadding (settings s))

drawPlayer :: GlobalState -> GridInfo -> Point -> Picture
drawPlayer gs gi (px, py) = let (w,h) = calcPlayerSize gs gi in translate px py $ resize 16 16 w h (getPlayerAnimation gs !! pFrame (player $ gameState gs))

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

confirmHighScorePrompt :: GlobalState -> String -> IO GlobalState
confirmHighScorePrompt s v
  | v /= "" = do
    let newState = s {highScores = insert v (score $ gameState s) (highScores s)}
    Data.Text.Lazy.IO.writeFile "assets/highscores.json" (encodeToLazyText $ highScores newState)
    return newState {route = LeaderBoardView, prompt = Nothing}
  | otherwise = do return s {route = LeaderBoardView, prompt = Nothing}
  where
    set = settings s


updateAnimationClocks :: GlobalState -> Float -> GlobalState
updateAnimationClocks s d | pauseGameTimer gs <= 0 && lives gs == 0 = s
              { prompt =
                  Just
                    defaultPrompt
                      { promptText = "Leaderboard Name: "
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

handleUpdateGameView :: Float -> GlobalState -> IO GlobalState
handleUpdateGameView f gs = do
  let updatedAnimationClocks = updateAnimationClocks gs f
  let updatedClocks = foldr (\g acc -> updateGhostClock acc f (getGhostActor gs g)) updatedAnimationClocks ghosts
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