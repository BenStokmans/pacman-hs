module Views.EditorView where

import Assets (Assets(Assets, emuFont, pacFont))
import Data.List
import Data.Maybe (fromMaybe, isJust, isNothing)
import FontContainer (FontContainer(..))
import Graphics.Gloss
  ( Color
  , Picture(..)
  , black
  , blank
  , blue
  , green
  , makeColor
  , orange
  , pictures
  , rectangleSolid
  , red
  , scale
  , translate
  , white
  , yellow
  )
import Graphics.Gloss.Data.Point ()
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), KeyState(..), MouseButton(..), SpecialKey(..))
import Map (WallType, getGhostSpawnPoint, getSpawnPoint, processWalls, wallToSizedSection)
import Rendering
  ( Rectangle(Rectangle)
  , cellSize
  , defaultButton
  , drawGrid
  , gridToScreenPos
  , rectangleHovered
  , renderButton
  , renderString
  , renderString'
  , renderStringTopLeft
  , screenToGridPos
  , stringSize
  )
import SDL.Font (Font(Font))
import State (EditorTool(..), GameState(..), GlobalState(..), MenuRoute(..), Prompt(blink), Settings(..), gridSizePx, getGhostActor)
import Struct (Cell(..), CellType(..), GhostBehaviour, GhostType(..), GridInfo, LevelMap(LevelMap), Vec2(..), getCell, ghosts, outOfBounds, setCell)
import System.Exit (exitSuccess)
import Text.Printf ()
import Views.GameView (debugGrid, drawGhost, drawMap, drawPlayer, getGhostColor, pelletColor)
import Views.PauseMenu (saveEditorLevel)
import Views.StartMenu (drawParticles, updateParticles)

generalIcon :: String -> Color -> Color -> GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
generalIcon s tc bc gs (x, y) w h = do
  ((_, _), l) <- renderString' (l $ emuFont $ assets gs) tc s
  return $ translate x y $ pictures [Color bc $ rectangleSolid w h, scale (w / 64) (h / 64) l]

wallIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
wallIcon = generalIcon "W" white blue

spawnIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
spawnIcon = generalIcon "S" black yellow

foodIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
foodIcon = generalIcon "F" white green

powerUpIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
powerUpIcon = generalIcon "P" white pelletColor

ghostWallIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
ghostWallIcon = generalIcon "U" white orange

blinkyIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
blinkyIcon = generalIcon "B" white (getGhostColor Blinky)

pinkyIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
pinkyIcon = generalIcon "P" white (getGhostColor Pinky)

inkyIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
inkyIcon = generalIcon "I" white (getGhostColor Inky)

clydeIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
clydeIcon = generalIcon "C" white (getGhostColor Clyde)

editorGrid :: GlobalState -> Picture
editorGrid s = drawGrid (getEditorGridInfo s) green

windowMargin :: (Float, Float) -> GlobalState -> (Float, Float)
windowMargin (c, r) s =
  let (gx, gy) = gridSizePx (c, r) s
   in (x - gx, y - gy)
  where
    (x, y) = windowSize (settings s)

getEditorGridInfo :: GlobalState -> GridInfo
getEditorGridInfo gs =
  let (Vec2 x y) = editorGridDimensions $ settings gs
   in ((x, y), gridSizePx (x, y) gs)

editorToolToIcon :: GlobalState -> (Float, Float) -> Float -> Float -> EditorTool -> IO Picture
editorToolToIcon gs pos w h WallTool = wallIcon gs pos w h
editorToolToIcon gs pos w h SpawnTool = spawnIcon gs pos w h
editorToolToIcon gs pos w h FoodTool = foodIcon gs pos w h
editorToolToIcon gs pos w h PowerUpTool = powerUpIcon gs pos w h
editorToolToIcon gs pos w h GhostWallTool = ghostWallIcon gs pos w h
editorToolToIcon gs pos w h GhostTool = ghostToIcon gs pos w h (editorGhost gs)

cellToIcon :: GlobalState -> Float -> Float -> Float -> Float -> CellType -> IO Picture
cellToIcon gs w h x y ct = do
  icon
  where
    icon
      | ct == Wall = wallIcon gs pos w h -- TODO: Clean this up
      | ct == Spawn = spawnIcon gs pos w h
      | ct == Pellet = foodIcon gs pos w h
      | ct == PowerUp = powerUpIcon gs pos w h
      | ct == GhostWall = ghostWallIcon gs pos w h
      | (GhostSpawn gt) <- ct = ghostToIcon gs pos w h gt
      | otherwise = do return blank
    pos = (x * w - (gw / 2) + w / 2, y * h - (gh / 2) + h / 2)
    (_, (gw, gh)) = getEditorGridInfo gs

ghostToIcon :: GlobalState -> (Float, Float) -> Float -> Float -> GhostType -> IO Picture
ghostToIcon gs pos w h Blinky = blinkyIcon gs pos w h
ghostToIcon gs pos w h Pinky = pinkyIcon gs pos w h
ghostToIcon gs pos w h Inky = inkyIcon gs pos w h
ghostToIcon gs pos w h Clyde = clydeIcon gs pos w h

previewButton :: Float -> Float -> Rectangle
previewButton mx my = Rectangle (mx, my) 325 40 10

saveButton :: Rectangle
saveButton = Rectangle (-320, -230) 120 40 10

renderEditorView :: GlobalState -> IO Picture
renderEditorView gs = do
  let mEmu = m (emuFont (assets gs))
  (_, fh) <- stringSize mEmu ""
  txt <- renderString (mx / 2, h / 2 + 25) mEmu red "Pac-Man Level Editor"
  toolsText <- renderStringTopLeft (-390, 320) mEmu white "Tools:" -- TODO: have all of these use top left rendering and properly alight them
  toolArrow <- renderStringTopLeft (-400, toolY) mEmu white "->"
  wallToolText <- renderStringTopLeft (-330, 290) mEmu white "wall"
  wallButton <- wallIcon gs (-350, 280) 25 25
  spawnToolText <- renderStringTopLeft (-330, 250) mEmu white "spawn"
  spawnButton <- spawnIcon gs (-350, 240) 25 25
  foodToolText <- renderStringTopLeft (-330, 210) mEmu white "food"
  foodButton <- foodIcon gs (-350, 200) 25 25
  powerUpToolText <- renderStringTopLeft (-330, 170) mEmu white "pow"
  powerUpButton <- powerUpIcon gs (-350, 160) 25 25
  ghostWallToolText <- renderStringTopLeft (-330, 130) mEmu white "gwall"
  ghostWallButton <- ghostWallIcon gs (-350, 120) 25 25
  ghostToolText <- renderStringTopLeft (-330, 90) mEmu white "ghost"
  ghostButton <- ghostToIcon gs (-350, 80) 25 25 (editorGhost gs)
  let sEmu = FontContainer.s (emuFont (assets gs))
  instructionText <-
    renderStringTopLeft
      (-390, 20)
      sEmu
      white
      "Select tool\nUsing arrow keys\n \nThen select\nAny square \nin the maze\nusing the mouse\n \nUse the space key\nto switch ghosts\n  \nLeft click: place\nRight click: del.\n    \nThe keys:\nW,A,F,U and S \nalso activate \ntheir respective\ntools."
  debugString <-
    renderStringTopLeft
      (-400, 400)
      sEmu
      green
      ("Hovered cell: " ++
       show mouseCell ++
       "\nCells occupied: " ++
       show (length cells) ++
       "\nMouse down: " ++
       mouseDebugText ++
       "\nGrids size: cells: " ++
       (let (LevelMap lw lh _) = editorLevel gs
         in show (Vec2 lw lh)) ++
       " pixels: " ++ show w ++ ", " ++ show h)
  previewButton <- defaultButton (previewButton (mx / 2) (-h / 2 - 30)) mEmu (previewText ++ " preview (V)") mPos
  saveButton <- defaultButton saveButton mEmu "Save" mPos
  let tools =
        pictures
          [ toolsText
          , toolArrow
          , wallButton
          , wallToolText
          , spawnButton
          , spawnToolText
          , foodToolText
          , foodButton
          , powerUpToolText
          , powerUpButton
          , ghostToolText
          , ghostWallButton
          , ghostWallToolText
          , ghostButton
          , instructionText
          , saveButton
          ]
  wi <- editorToolToIcon gs (x * cw - (w / 2) + cw / 2, y * ch - (h / 2) + ch / 2) cw ch tool
  let hoveredCell =
        if x < c && x >= 0 && y < r && y >= 0
          then wi
          else blank -- check if hovered cell is on the grid
  cs <- mapM (\(Cell t (Vec2 vx vy)) -> cellToIcon gs cw ch vx vy t) (concat cells)
  let gridEditor =
        pictures
          [ pictures cs
          , if not rightDown
              then hoveredCell
              else blank
          , editorGrid gs
          ]
  let wallsPreview = drawMap gs {cachedWalls = processWalls level} level dims
  let pacmanPreview =
        let pacSpawn = getSpawnPoint level
         in if pacSpawn == outOfBounds
              then blank
              else drawPlayer gs dims $ gridToScreenPos dims pacSpawn
  let ghostPreviews =
        pictures $
        map
          ((\(t, v) ->
              if v == outOfBounds
                then blank
                else drawGhost gs t dims (gridToScreenPos dims v)) .
           (\t -> (getGhostActor gs t, getGhostSpawnPoint level t)))
          ghosts
  let gridPreview = pictures [wallsPreview, pacmanPreview, ghostPreviews]
  let grid =
        translate (mx / 2) 0 $
        if previewEditor gs
          then gridPreview
          else gridEditor
  return (pictures [grid, txt, tools, if debugEnabled $ settings gs then debugString else blank, previewButton])
  where
    dims@((c, r), (w, h)) = getEditorGridInfo gs
    level@(LevelMap _ _ cells) = editorLevel gs
    mPos@(mouseX, mouseY) = mousePos gs
    mouseCell@(Vec2 x y) = screenToGridPos dims (mouseX - mx / 2, mouseY)
    (cw, ch) = cellSize dims
    (mx, _) = windowMargin (c, r) gs
    tool = editorTool gs
    previewText =
      if previewEditor gs
        then "Disable"
        else "Enable"
    toolY
      | tool == WallTool = 290
      | tool == SpawnTool = 250
      | tool == FoodTool = 210
      | tool == PowerUpTool = 170
      | tool == GhostWallTool = 130
      | tool == GhostTool = 90
      | otherwise = -1000 -- out of bounds
    rightDown = MouseButton RightButton `elem` keys gs
    leftDown = MouseButton LeftButton `elem` keys gs
    mouseDebugText
      | leftDown && rightDown = "left + right"
      | leftDown = "left"
      | rightDown = "right"
      | otherwise = "no"

tools :: [EditorTool]
tools = [WallTool, SpawnTool, FoodTool, PowerUpTool, GhostWallTool, GhostTool]

getNextCircular :: Eq a => a -> [a] -> a
getNextCircular v xs
  | null rest = head xs
  | otherwise = head rest
  where
    rest = tail $ dropWhile (/= v) xs

nextTool :: EditorTool -> EditorTool
nextTool t = getNextCircular t tools

prevTool :: EditorTool -> EditorTool
prevTool t = getNextCircular t (reverse tools)

nextGhost :: GhostType -> GhostType
nextGhost t = getNextCircular t ghosts

toolToCellType :: EditorTool -> GhostType -> CellType
toolToCellType WallTool _ = Wall
toolToCellType SpawnTool _ = Spawn
toolToCellType FoodTool _ = Pellet
toolToCellType PowerUpTool _ = PowerUp
toolToCellType GhostWallTool _ = GhostWall
toolToCellType GhostTool gt = GhostSpawn gt

charToTool :: EditorTool -> Char -> EditorTool
charToTool _ 'w' = WallTool
charToTool _ 'p' = PowerUpTool
charToTool _ 'f' = FoodTool
charToTool _ 's' = SpawnTool
charToTool _ 'g' = GhostTool
charToTool _ 'u' = GhostWallTool
charToTool e _ = e

handleInputEditorView :: Event -> GlobalState -> IO GlobalState
handleInputEditorView (EventKey (SpecialKey KeyEsc) _ _ _) gs = do
  return gs {route = PauseMenu, lastRoute = EditorView}
handleInputEditorView (EventKey (SpecialKey KeyUp) _ _ _) gs@(GlobalState {editorTool = et}) = do
  return gs {editorTool = prevTool et}
handleInputEditorView (EventKey (SpecialKey KeyDown) _ _ _) gs@(GlobalState {editorTool = et}) = do
  return gs {editorTool = nextTool et}
handleInputEditorView (EventKey (SpecialKey KeySpace) _ _ _) gs@(GlobalState {editorGhost = et, editorTool = GhostTool}) = do
  return gs {editorGhost = nextGhost et}
handleInputEditorView (EventKey (Char 'v') _ _ _) gs = do
  return gs {previewEditor = not $ previewEditor gs}
handleInputEditorView (EventKey (Char c) _ _ _) gs = do
  return gs {editorTool = charToTool (editorTool gs) c}
handleInputEditorView (EventKey (MouseButton LeftButton) _ _ _) gs
  | rectangleHovered (mousePos gs) saveButton = do
    saveEditorLevel gs
    return gs
  | rectangleHovered (mousePos gs) $ previewButton mx (-h / 2 - 30) = do return gs {previewEditor = not $ previewEditor gs}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 260) 25 25 0 = do return gs {editorTool = WallTool}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 220) 25 25 0 = do return gs {editorTool = SpawnTool}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 180) 25 25 0 = do return gs {editorTool = FoodTool}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 140) 25 25 0 = do return gs {editorTool = PowerUpTool}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 100) 25 25 0 = do return gs {editorTool = GhostWallTool}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 60) 25 25 0 = do return gs {editorTool = GhostTool}
  where
    (dim, (_, h)) = getEditorGridInfo gs
    (mx, _) = windowMargin dim gs
handleInputEditorView _ gs = do
  return gs

handleUpdateEditorView :: Float -> GlobalState -> IO GlobalState
handleUpdateEditorView _ s = do
  return newState
  where
    dims@((c, r), (w, h)) = getEditorGridInfo s
    (mx, _) = windowMargin (c, r) s
    (mouseX, mouseY) = mousePos s
    v@(Vec2 x y) = screenToGridPos dims (mouseX - mx / 2, mouseY)
    m@(LevelMap _ _ cells) = editorLevel s
    mCell = getCell m v
    emptyCell = Cell Empty v
    newCell = Cell (toolToCellType (editorTool s) (editorGhost s)) v
    newState
      | previewEditor s = s
      | x >= c || x < 0 || y >= r || y < 0 = s
      | elem (MouseButton LeftButton) $ keys s = s {editorLevel = setCell m newCell}
      | elem (MouseButton RightButton) $ keys s = s {editorLevel = setCell m emptyCell}
      | otherwise = s
