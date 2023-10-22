module Views.EditorView where

import Assets (Assets (Assets, emuFont, pacFont))
import Data.List
import Data.Maybe (fromMaybe, isJust, isNothing)
import FontContainer (FontContainer (..))
import Graphics.Gloss (Color, Picture (..), black, blank, blue, green, pictures, rectangleSolid,
                       red, scale, translate, white, yellow)
import Graphics.Gloss.Data.Point ()
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), MouseButton (..),
                                         SpecialKey (..))
import Map (WallType, processWalls, wallToSizedSection)
import Rendering (Rectangle (Rectangle), defaultButton, rectangleHovered, renderButton,
                  renderString, renderString', renderStringTopLeft)
import SDL.Font (Font (Font))
import State (EditorTool (..), GameState (..), GlobalState (..), MenuRoute (..), Settings (..))
import Struct (Cell (..), CellType (..), LevelMap (LevelMap), Vec2 (..), getCell)
import System.Exit (exitSuccess)
import Text.Printf
import Views.GameView (cellSize, debugGrid, drawGrid, drawMap, gridSizePx, screenToGridPos)
import Views.StartMenu (drawParticles, updateParticles)

generalIcon :: String -> Color -> Color -> GlobalState -> (Float,Float) -> Float -> Float -> IO Picture
generalIcon s tc bc gs (x,y) w h = do
    ((_,_),l) <- renderString' (l $ emuFont $ assets gs) tc s
    return $ translate x y $ pictures [Color bc $ rectangleSolid w h, scale (w/64) (h/64) l ]

wallIcon :: GlobalState -> (Float,Float) -> Float -> Float -> IO Picture
wallIcon = generalIcon "W" white blue

spawnIcon :: GlobalState -> (Float,Float) -> Float -> Float -> IO Picture
spawnIcon = generalIcon "P" black yellow

foodIcon :: GlobalState -> (Float,Float) -> Float -> Float -> IO Picture
foodIcon = generalIcon "F" white green

appleIcon :: GlobalState -> (Float,Float) -> Float -> Float -> IO Picture
appleIcon = generalIcon "A" white red

editorGrid :: GlobalState -> Picture
editorGrid s = let (w,h) = gridSizePx dim s in drawGrid dim w h green
    where
        dim = let (Vec2 x y) = editorGridDimensions $ settings s in (x,y)

windowMargin :: (Float,Float) -> GlobalState -> (Float,Float)
windowMargin (c,r) s = let (gx,gy) = gridSizePx (c,r) s in (x-gx,y-gy)
    where
        (x,y) = windowSize (settings s)

--                                     c     r     w     h
getEditorGridInfo :: GlobalState -> ((Float,Float),(Float,Float))
getEditorGridInfo gs = let (Vec2 x y) = editorGridDimensions $ settings gs in ((x,y), gridSizePx (x,y) gs)

editorToolToIcon :: GlobalState -> (Float,Float) -> Float -> Float -> EditorTool -> IO Picture
editorToolToIcon gs (x,y) w h WallTool  = wallIcon gs (x,y) w h
editorToolToIcon gs (x,y) w h SpawnTool = spawnIcon gs (x,y) w h
editorToolToIcon gs (x,y) w h FoodTool  = foodIcon gs (x,y) w h
editorToolToIcon gs (x,y) w h AppleTool = appleIcon gs (x,y) w h

cellToIcon :: GlobalState -> Float -> Float -> Float -> Float -> CellType -> IO Picture
cellToIcon gs w h x y ct = do icon
                where icon | ct == Wall = wallIcon gs pos w h
                           | ct == Spawn = spawnIcon gs pos w h
                           | ct == Pellet = foodIcon gs pos w h
                           | ct == PowerUp = appleIcon gs pos w h
                           | otherwise = do return blank
                      pos = (x*w-(gw/2)+w/2,y*h-(gh/2)+h/2)
                      (_,(gw,gh)) = getEditorGridInfo gs

previewButton :: Float -> Float -> Rectangle
previewButton mx my = Rectangle (mx, my) 325 40 10

renderEditorView :: GlobalState -> IO Picture
renderEditorView gs = do
    let mEmu = m (emuFont (assets gs))
    txt <- renderString (mx/2,h/2+25) mEmu red "Pac-Man Level Editor"
    toolsText <- renderString (-350, 300) mEmu white "Tools:"
    toolArrow <- renderString (-380, toolY) mEmu white "->"

    wallToolText <- renderString (-300, 260) mEmu white "wall"
    wallButton <- wallIcon gs (-350,260) 25 25

    spawnToolText <- renderString (-290, 220) mEmu white "spawn"
    spawnButton <- spawnIcon gs (-350, 220) 25 25

    foodToolText <- renderString (-300, 180) mEmu white "food"
    foodButton <- foodIcon gs (-350, 180) 25 25

    appleToolText <- renderString (-290, 140) mEmu white "apple"
    appleButton <- appleIcon gs (-350, 140) 25 25

    let sEmu = FontContainer.s (emuFont (assets gs))
    instructionText <- renderStringTopLeft (-390,100) sEmu white "Select tool\nUsing arrow keys\n \nThen select\nAny square \nin the maze\nusing the mouse\n  \nLeft click: place\nRight click: del.\n    \nThe keys:\nW,A,F and P \nalso activate \ntheir respective\ntools."
    debugString <- renderStringTopLeft (-400,400) sEmu green
                ("Hovered cell: " ++ show v ++ "\nCells occupied: " ++ show (length cells) ++ "\nMouse down: " ++ mouseDebugText ++ "\n Grids size: cells: " ++ (let (LevelMap lw lh _) = editorLevel gs in show (Vec2 lw lh)) ++ " pixels: " ++ show w ++ ", " ++ show h)
    cs <- mapM (\(Cell t (Vec2 vx vy)) -> cellToIcon gs cw ch vx vy t) cells

    previewButton <- defaultButton (previewButton (mx/2) (-h/2-30)) mEmu (previewText ++ " preview (V)") mPos
    let tools = pictures [toolsText,toolArrow,wallButton,wallToolText,spawnButton,spawnToolText,foodToolText,foodButton,appleToolText,appleButton,instructionText]
    wi <- editorToolToIcon gs (x*cw-(w/2)+cw/2, y*ch-(h/2)+ch/2) cw ch tool

    let hoveredCell = if x<c && x>=0 && y<r && y>=0 then wi else blank -- check if hovered cell is on the grid
    let gridEditor = translate (mx/2) 0 $ pictures [pictures cs,if not rightDown then hoveredCell else blank,editorGrid gs]
    let gridPreview = translate (mx/2) 0 $ drawMap gs level (c,r) (w,h)
    return (pictures [if previewEditor gs then gridPreview else gridEditor,txt,tools,debugString,previewButton])
    where
        ((c,r),(w,h)) = getEditorGridInfo gs
        level@(LevelMap _ _ cells) = editorLevel gs
        mPos@(mouseX,mouseY) = mousePos gs
        v@(Vec2 x y) = screenToGridPos gs (c,r) (mouseX - mx/2, mouseY)
        (cw,ch) = cellSize (c,r) w h
        (mx,_) = windowMargin (c,r) gs
        tool = editorTool gs
        previewText = if previewEditor gs then "Disable" else "Enable"
        toolY
            | tool == WallTool = 260
            | tool == SpawnTool = 220
            | tool == FoodTool = 180
            | tool == AppleTool = 140
            | otherwise = -1000 -- out of bounds
        rightDown = MouseButton RightButton `elem` keys gs
        leftDown = MouseButton LeftButton `elem` keys gs
        mouseDebugText | leftDown && rightDown = "left + right"
                       | leftDown = "left"
                       | rightDown = "right"
                       | otherwise = "no"

tools :: [EditorTool]
tools = [WallTool,SpawnTool,FoodTool,AppleTool]

toolRec' :: EditorTool -> [EditorTool] -> EditorTool
toolRec' v xs | null rest = head xs
              | otherwise = head rest
            where
              rest = tail $ dropWhile (/=v) xs

nextTool :: EditorTool -> EditorTool
nextTool t = toolRec' t tools

prevTool :: EditorTool -> EditorTool
prevTool t = toolRec' t (reverse tools)

toolToCellType :: EditorTool -> CellType
toolToCellType WallTool  = Wall
toolToCellType SpawnTool = Spawn
toolToCellType FoodTool  = Pellet
toolToCellType AppleTool = PowerUp

charToTool :: EditorTool -> Char -> EditorTool
charToTool _ 'w' = WallTool
charToTool _ 'a' = AppleTool
charToTool _ 'f' = FoodTool
charToTool _ 'p' = SpawnTool
charToTool e _   = e

handleInputEditorView :: Event -> GlobalState -> IO GlobalState
handleInputEditorView (EventKey (SpecialKey KeyEsc) _ _ _) gs = do return gs {route = PauseMenu, lastRoute = EditorView}
handleInputEditorView (EventKey (SpecialKey KeyUp) _ _ _) gs@(GlobalState {editorTool = et}) = do return gs {editorTool = prevTool et}
handleInputEditorView (EventKey (SpecialKey KeyDown) _ _ _) gs@(GlobalState {editorTool = et}) = do return gs {editorTool = nextTool et}
handleInputEditorView (EventKey (Char 'v') _ _ _ ) gs = do return gs {previewEditor = not $ previewEditor gs}
handleInputEditorView (EventKey (Char c) _ _ _ ) gs = do return gs { editorTool = charToTool (editorTool gs) c }
handleInputEditorView (EventKey (MouseButton LeftButton) _ _ _) gs
    | rectangleHovered (mousePos gs) $ previewButton mx (-h/2-30) = do return gs {previewEditor = not $ previewEditor gs}
    | rectangleHovered (mousePos gs) $ Rectangle (-350, 260) 25 25 0 = do return gs {editorTool = WallTool}
    | rectangleHovered (mousePos gs) $ Rectangle (-350, 220) 25 25 0 = do return gs {editorTool = SpawnTool}
    | rectangleHovered (mousePos gs) $ Rectangle (-350, 180) 25 25 0 = do return gs {editorTool = FoodTool}
    | rectangleHovered (mousePos gs) $ Rectangle (-350, 140) 25 25 0 = do return gs {editorTool = AppleTool}
    where
        (dim,(_,h)) = getEditorGridInfo gs
        (mx,_) = windowMargin dim gs
handleInputEditorView _ gs = do return gs

handleUpdateEditorView :: Float -> GlobalState -> IO GlobalState
handleUpdateEditorView _ s = do
    return newState
    where
        (dim@(c,r),(w,h)) = getEditorGridInfo s
        (mx,_) = windowMargin dim s
        (mouseX,mouseY) = mousePos s
        v@(Vec2 x y) = screenToGridPos s dim (mouseX - mx/2, mouseY)

        map@(LevelMap _ _ cells) = editorLevel s
        mCell = getCell map v
        cell = fromMaybe (Cell Empty (Vec2 0 0)) mCell

        editCells | isJust mCell = delete cell cells
                  | otherwise = cells

        newCell = Cell (toolToCellType $ editorTool s) v
        newState | x>=c || x<0 || y>=r || y<0 = s
                 | elem (MouseButton LeftButton) $ keys s = s { editorLevel = LevelMap c r $ newCell:editCells }
                 | elem (MouseButton RightButton) $ keys s = s { editorLevel = LevelMap c r editCells }
                 | otherwise = s
