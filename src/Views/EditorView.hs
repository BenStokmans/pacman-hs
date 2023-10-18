module Views.EditorView where

import State (GlobalState(..), MenuRoute (..), MenuRoute(StartMenu), GameState (..), Settings (..), EditorTool (..))
import Assets(Assets(Assets,pacFont, emuFont))
import FontContainer(FontContainer(..))
import Rendering(renderString,renderButton, rectangleHovered, Rectangle (Rectangle), renderString', renderStringTopLeft)
import Graphics.Gloss
    ( Picture,
      blue,
      red,
      white,
      pictures,
      scale,
      rectangleSolid,
      translate,
      Picture(..), green, yellow, Color, black, blank )
import Graphics.Gloss.Interface.IO.Game ( Event (..), Key (..), MouseButton (..), SpecialKey (..), KeyState (..) )
import Graphics.Gloss.Data.Point ()
import System.Exit (exitSuccess)
import Views.StartMenu (drawParticles, updateParticles)
import Views.GameView (debugGrid, screenToGridPos, gridSizePx, cellSize, gridSize, drawGrid)
import Struct (Vec2(..), getCell, Cell (..), CellType (..), LevelMap (LevelMap))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.List
import Map (WallType)
import Text.Printf
import SDL.Font (Font(Font))

generalIcon :: String -> Color -> Color -> GlobalState -> Float -> Float -> IO Picture
generalIcon s tc bc gs w h = do
    ((_,_),l) <- renderString' (l $ emuFont $ assets gs) tc s
    return $ pictures [Color bc $ rectangleSolid w h, scale (w/64) (h/64) l ]

wallIcon :: GlobalState -> Float -> Float -> IO Picture
wallIcon = generalIcon "W" white blue

spawnIcon :: GlobalState -> Float -> Float -> IO Picture
spawnIcon = generalIcon "P" black yellow

foodIcon :: GlobalState -> Float -> Float -> IO Picture
foodIcon = generalIcon "F" white green

appleIcon :: GlobalState -> Float -> Float -> IO Picture
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

editorToolToIcon :: GlobalState -> Float -> Float -> EditorTool -> IO Picture
editorToolToIcon gs w h WallTool = wallIcon gs w h
editorToolToIcon gs w h SpawnTool = spawnIcon gs w h
editorToolToIcon gs w h FoodTool = foodIcon gs w h
editorToolToIcon gs w h AppleTool = appleIcon gs w h

cellToIcon :: GlobalState -> Float -> Float -> Float -> Float -> CellType -> IO Picture
cellToIcon gs w h x y ct = do translate (x*w-(gw/2)+w/2) (y*h-(gh/2)+h/2) <$> icon
                where icon | ct == Wall = wallIcon gs w h
                           | ct == Spawn = spawnIcon gs w h
                           | ct == Pellet = foodIcon gs w h
                           | otherwise = do return blank
                      (_,(gw,gh)) = getEditorGridInfo gs

renderEditorView :: GlobalState -> IO Picture
renderEditorView s = do
    txt <- renderString (0,350) (m (emuFont (assets s))) red "Pac-Man Level Editor"
    toolsText <- renderString (-350, 300) (m (emuFont (assets s))) white "Tools:"
    toolArrow <- renderString (-380, toolY) (m (emuFont (assets s))) white "->"

    wallToolText <- renderString (-300, 260) (m (emuFont (assets s))) white "wall"
    wallButtonIO <- wallIcon s 25 25
    let wallButton = translate (-350) 260 wallButtonIO

    spawnToolText <- renderString (-290, 220) (m (emuFont (assets s))) white "spawn"
    spawnButtonIO <- spawnIcon s 25 25
    let spawnButton = translate (-350) 220 spawnButtonIO

    foodToolText <- renderString (-300, 180) (m (emuFont (assets s))) white "food"
    foodButtonIO <- foodIcon s 25 25
    let foodButton = translate (-350) 180 foodButtonIO

    appleToolText <- renderString (-290, 140) (m (emuFont (assets s))) white "apple"
    appleButtonIO <- appleIcon s 25 25
    let appleButton = translate (-350) 140 appleButtonIO

    instructionText <- renderStringTopLeft (-390,100) (FontContainer.s (emuFont (assets s))) white "Select tool\nUsing arrow keys\n \nThen select\nAny square \nin the maze\nusing the mouse\n  \nLeft click: place\nRight click: del.\n    \nThe keys:\nW,A,F and P \nalso activate \ntheir respective\ntools."
    debugString <- renderStringTopLeft (-400,400) (FontContainer.s (emuFont (assets s))) green
                ("Hovered cell: " ++ show v ++ "\nCells occupied: " ++ show (length cells) ++ "\nMouse down: " ++ mouseDebugText)
    cs <- mapM (\(Cell t (Vec2 vx vy)) -> cellToIcon s cw ch vx vy t) cells

    let tools = pictures [toolsText,toolArrow,wallButton,wallToolText,spawnButton,spawnToolText,foodToolText,foodButton,appleToolText,appleButton,instructionText]
    wi <- editorToolToIcon s cw ch tool
    let hoveredCell = if x<c && x>=0 && y<r && y>=0 then translate (x*cw-(w/2)+cw/2) (y*ch-(h/2)+ch/2) wi else blank -- check if hovered cell is on the grid
    let gridStuff = translate (mx/2) 0 $ pictures [pictures cs,hoveredCell,editorGrid s] -- give beter name
    return (pictures [gridStuff,txt,tools,debugString])
    where
        ((c,r),(w,h)) = getEditorGridInfo s
        (LevelMap _ _ cells) = editorLevel s
        (mouseX,mouseY) = mousePos s
        v@(Vec2 x y) = screenToGridPos s (c,r) (mouseX - mx/2, mouseY)
        (cw,ch) = cellSize (c,r) w h
        (mx,_) = windowMargin (c,r) s
        tool = editorTool s
        toolY
            | tool == WallTool = 260
            | tool == SpawnTool = 220
            | tool == FoodTool = 180
            | tool == AppleTool = 140
            | otherwise = -1000 -- out of bounds
        mouseDebugText | elem (MouseButton LeftButton) (keys s) && elem (MouseButton RightButton) (keys s) = "left + right"
                       | elem (MouseButton LeftButton) $ keys s = "left"  -- crazy inneficient but it's only debug so eh
                       | elem (MouseButton LeftButton) $ keys s = "right"
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
toolToCellType WallTool = Wall
toolToCellType SpawnTool = Spawn
toolToCellType FoodTool = Pellet
toolToCellType AppleTool = Empty -- add powerup

charToTool :: Char -> EditorTool
charToTool 'w' = WallTool
charToTool 'a' = AppleTool
charToTool 'f' = FoodTool
charToTool 'p' = SpawnTool

handleInputEditorView :: Event -> GlobalState -> IO GlobalState
handleInputEditorView (EventKey (SpecialKey KeyEsc) _ _ _) s = do return s {route = PauseMenu, lastRoute = EditorView}
handleInputEditorView (EventKey (SpecialKey KeyUp) _ _ _) s@(GlobalState {editorTool = et}) = do return s {editorTool = prevTool et}
handleInputEditorView (EventKey (SpecialKey KeyDown) _ _ _) s@(GlobalState {editorTool = et}) = do return s {editorTool = nextTool et}
handleInputEditorView (EventKey (Char c) _ _ _ ) s = do return s { editorTool = charToTool c }
handleInputEditorView _ s = do return s

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
                 | elem (MouseButton LeftButton) $ keys s = s { editorLevel = LevelMap w h $ newCell:editCells }
                 | elem (MouseButton RightButton) $ keys s = s { editorLevel = LevelMap w h editCells }
                 | otherwise = s