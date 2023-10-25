module Views.EditorView where

import Assets (Assets(Assets, emuFont, pacFont))
import Data.List
import Data.Maybe (fromMaybe, isJust, isNothing)
import FontContainer (FontContainer(..))
import Graphics.Gloss (Color, Picture(..), black, blank, blue, green, makeColor, pictures, rectangleSolid, red, scale, translate, white, yellow)
import Graphics.Gloss.Data.Point ()
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), KeyState(..), MouseButton(..), SpecialKey(..))
import Map (WallType, getGhostSpawnPoint, getSpawnPoint, processWalls, wallToSizedSection)
import Rendering (Rectangle(Rectangle), defaultButton, rectangleHovered, renderButton, renderString, renderString', renderStringTopLeft, gridToScreenPos, cellSize)
import SDL.Font (Font(Font))
import State (EditorTool(..), GameState(..), GlobalState(..), MenuRoute(..), Prompt(blink), Settings(..))
import Struct (Cell(..), CellType(..), GhostBehaviour, GhostType(..), GridInfo, LevelMap(LevelMap), Vec2(..), getCell, ghosts, outOfBounds)
import System.Exit (exitSuccess)
import Text.Printf ()
import Views.GameView (debugGrid, drawGhost, drawGrid, drawMap, drawPlayer, gridSizePx, screenToGridPos)
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

appleIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
appleIcon = generalIcon "A" white red

blinkyIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
blinkyIcon = generalIcon "B" white red

pinkyIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
pinkyIcon = generalIcon "P" white (makeColor 1 0.72 1 1)

inkyIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
inkyIcon = generalIcon "I" white (makeColor 0 1 1 1)

clydeIcon :: GlobalState -> (Float, Float) -> Float -> Float -> IO Picture
clydeIcon = generalIcon "C" white (makeColor 1 0.72 0.32 1)

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
editorToolToIcon gs pos w h AppleTool = appleIcon gs pos w h
editorToolToIcon gs pos w h GhostTool = ghostToIcon gs pos w h (editorGhost gs)

cellToIcon :: GlobalState -> Float -> Float -> Float -> Float -> CellType -> IO Picture
cellToIcon gs w h x y ct = do
  icon
  where
    icon
      | ct == Wall = wallIcon gs pos w h
      | ct == Spawn = spawnIcon gs pos w h
      | ct == Pellet = foodIcon gs pos w h
      | ct == PowerUp = appleIcon gs pos w h
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

renderEditorView :: GlobalState -> IO Picture
renderEditorView gs = do
  let mEmu = m (emuFont (assets gs))
  txt <- renderString (mx / 2, h / 2 + 25) mEmu red "Pac-Man Level Editor"
  toolsText <- renderString (-350, 300) mEmu white "Tools:"
  toolArrow <- renderString (-380, toolY) mEmu white "->"
  wallToolText <- renderString (-300, 260) mEmu white "wall"
  wallButton <- wallIcon gs (-350, 260) 25 25
  spawnToolText <- renderString (-290, 220) mEmu white "spawn"
  spawnButton <- spawnIcon gs (-350, 220) 25 25
  foodToolText <- renderString (-300, 180) mEmu white "food"
  foodButton <- foodIcon gs (-350, 180) 25 25
  appleToolText <- renderString (-290, 140) mEmu white "apple"
  appleButton <- appleIcon gs (-350, 140) 25 25
  ghostToolText <- renderString (-290, 100) mEmu white "ghost"
  ghostButton <- ghostToIcon gs (-350, 100) 25 25 (editorGhost gs)
  let sEmu = FontContainer.s (emuFont (assets gs))
  instructionText <-
    renderStringTopLeft
      (-390, 60)
      sEmu
      white
      "Select tool\nUsing arrow keys\n \nThen select\nAny square \nin the maze\nusing the mouse\n \nUse the space key\nto switch ghosts\n  \nLeft click: place\nRight click: del.\n    \nThe keys:\nW,A,F and S \nalso activate \ntheir respective\ntools."
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
       "\n Grids size: cells: " ++
       (let (LevelMap lw lh _) = editorLevel gs
         in show (Vec2 lw lh)) ++
       " pixels: " ++ show w ++ ", " ++ show h)
  previewButton <- defaultButton (previewButton (mx / 2) (-h / 2 - 30)) mEmu (previewText ++ " preview (V)") mPos
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
          , appleToolText
          , appleButton
          , ghostToolText
          , ghostButton
          , instructionText
          ]
  wi <- editorToolToIcon gs (x * cw - (w / 2) + cw / 2, y * ch - (h / 2) + ch / 2) cw ch tool
  let hoveredCell =
        if x < c && x >= 0 && y < r && y >= 0
          then wi
          else blank -- check if hovered cell is on the grid
  cs <- mapM (\(Cell t (Vec2 vx vy)) -> cellToIcon gs cw ch vx vy t) cells
  let gridEditor =
        pictures
          [ pictures cs
          , if not rightDown
              then hoveredCell
              else blank
          , editorGrid gs
          ]
  let wallsPreview = drawMap gs {cachedWalls = processWalls level} level dims
  let pacmanPreview = let pacSpawn = getSpawnPoint level in
            if pacSpawn == outOfBounds
              then blank
              else drawPlayer gs dims $ gridToScreenPos dims pacSpawn
  let ghostPreviews =
        pictures $
        map
          ((\(t, v) ->
              if v == outOfBounds
                then blank
                else drawGhost gs t dims (gridToScreenPos dims v)) .
           (\t -> (t, getGhostSpawnPoint level t)))
          ghosts
  let gridPreview = pictures [wallsPreview, pacmanPreview, ghostPreviews]
  let grid =
        translate (mx / 2) 0 $
        if previewEditor gs
          then gridPreview
          else gridEditor
  return (pictures [grid, txt, tools, debugString, previewButton])
  where
    dims@((c, r), (w, h)) = getEditorGridInfo gs
    level@(LevelMap _ _ cells) = editorLevel gs
    mPos@(mouseX, mouseY) = mousePos gs
    mouseCell@(Vec2 x y) = screenToGridPos gs dims (mouseX - mx / 2, mouseY)
    (cw, ch) = cellSize dims
    (mx, _) = windowMargin (c, r) gs
    tool = editorTool gs
    previewText =
      if previewEditor gs
        then "Disable"
        else "Enable"
    toolY
      | tool == WallTool = 260
      | tool == SpawnTool = 220
      | tool == FoodTool = 180
      | tool == AppleTool = 140
      | tool == GhostTool = 100
      | otherwise = -1000 -- out of bounds
    rightDown = MouseButton RightButton `elem` keys gs
    leftDown = MouseButton LeftButton `elem` keys gs
    mouseDebugText
      | leftDown && rightDown = "left + right"
      | leftDown = "left"
      | rightDown = "right"
      | otherwise = "no"

tools :: [EditorTool]
tools = [WallTool, SpawnTool, FoodTool, AppleTool, GhostTool]

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
toolToCellType AppleTool _ = PowerUp
toolToCellType GhostTool gt = GhostSpawn gt

charToTool :: EditorTool -> Char -> EditorTool
charToTool _ 'w' = WallTool
charToTool _ 'a' = AppleTool
charToTool _ 'f' = FoodTool
charToTool _ 's' = SpawnTool
charToTool _ 'g' = GhostTool
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
  | rectangleHovered (mousePos gs) $ previewButton mx (-h / 2 - 30) = do return gs {previewEditor = not $ previewEditor gs}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 260) 25 25 0 = do return gs {editorTool = WallTool}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 220) 25 25 0 = do return gs {editorTool = SpawnTool}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 180) 25 25 0 = do return gs {editorTool = FoodTool}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 140) 25 25 0 = do return gs {editorTool = AppleTool}
  | rectangleHovered (mousePos gs) $ Rectangle (-350, 100) 25 25 0 = do return gs {editorTool = GhostTool}
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
    v@(Vec2 x y) = screenToGridPos s dims (mouseX - mx / 2, mouseY)
    map@(LevelMap _ _ cells) = editorLevel s
    mCell = getCell map v
    cell = fromMaybe (Cell Empty (Vec2 0 0)) mCell
    editCells
      | isJust mCell = delete cell cells
      | otherwise = cells
    newCell = Cell (toolToCellType (editorTool s) (editorGhost s)) v
    newState
      | x >= c || x < 0 || y >= r || y < 0 = s
      | elem (MouseButton LeftButton) $ keys s = s {editorLevel = LevelMap c r $ newCell : editCells}
      | elem (MouseButton RightButton) $ keys s = s {editorLevel = LevelMap c r editCells}
      | otherwise = s
