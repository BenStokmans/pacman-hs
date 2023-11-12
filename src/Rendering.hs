module Rendering where

import Control.Exception (bracket, bracket_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (intercalate, pack)
import Data.Word (Word8)
import Foreign (castPtr, copyBytes, finalizeForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.C.Types (CInt(..))
import Graphics.Gloss
  ( Color
  , Picture(Bitmap, Color)
  , Point
  , black
  , blank
  , blue
  , pictures
  , rectangleSolid
  , rectangleWire
  , rgbaOfColor
  , scale
  , translate
  , white
  )
import Graphics.Gloss.Data.Point (Point, pointInBox)
import Graphics.Gloss.Rendering (BitmapData(..), BitmapFormat(..), Picture(..), PixelFormat(..), RowOrder(..), bitmapDataOfForeignPtr, bitmapSize)
import Linear.V2 (V2(..))
import SDL.Font (Font, blended, size)
import SDL.Vect (V4(..))
import SDL.Video.Renderer
  ( PixelFormat(RGBA8888)
  , Surface
  , createRGBSurface
  , freeSurface
  , lockSurface
  , surfaceBlit
  , surfaceDimensions
  , surfacePixels
  , unlockSurface
  )
import Text.Printf (printf)
import GameLogic.MapLogic
    ( Cell(..), Vec2(..), GridInfo, cellSize, gridToScreenPos )
import GameLogic.MapRendering
    ( WallSection, defaultSize, wallSectionToPic )

data Rectangle =
  Rectangle Point Float Float Float --Centre point, width, height, border thickness

wallToSizedSection :: Float -> Float -> Float -> Float -> WallSection -> Picture
wallToSizedSection m t nw nh ws =
  let (ow, oh) = defaultSize
   in resize ow oh nw nh (wallSectionToPic m t ow oh ws)
   
calcSpriteSize :: GridInfo -> (Float, Float) -> Float -> (Float, Float)
calcSpriteSize gi@((c, r), _) (w, h) scalar = let (cw, ch) = cellSize gi in (w * (cw / w * scalar * (c / r)), h * (ch / h * scalar * (r / c)))

calcSprite16Size :: GridInfo -> Float -> (Float, Float)
calcSprite16Size gi = calcSpriteSize gi (16,16)

calcSprite32Size :: GridInfo -> Float -> (Float, Float)
calcSprite32Size gi = calcSpriteSize gi (32,32)

drawGrid :: GridInfo -> Color -> Picture
drawGrid gi@((c, r), (w, h)) col =
  Color col $
  pictures $
  [ let hc = -w2 + cw * i
   in Line [(hc, -h2), (hc, h2)]
  | i <- [0 .. c]
  ] ++
  [ let hr = -h2 + ch * i
   in Line [(-w2, hr), (w2, hr)]
  | i <- [0 .. r]
  ]
  where
    w2 = w / 2
    h2 = h / 2
    (cw, ch) = cellSize gi

screenToGridPos :: GridInfo -> Point -> Vec2 -- get position on grid from screen position
screenToGridPos gi@(_, (pw, ph)) (x, y) = Vec2 (fromIntegral (floor ((pw / 2 + x) / cw))) (fromIntegral (floor ((ph / 2 + y) / ch)))
  where
    (cw, ch) = cellSize gi

translateCell :: Cell -> ((Float, Float), (Float, Float)) -> Picture -> Picture
translateCell (Cell _ v) gi =
  let (x, y) = gridToScreenPos gi v
   in translate x y

resize :: Float -> Float -> Float -> Float -> Picture -> Picture
resize ow oh nw nh = scale (nw / ow) (nh / oh)

rectangleHovered :: Point -> Rectangle -> Bool
rectangleHovered mouse (Rectangle (x, y) width height _) = pointInBox mouse (sx + x, -sy + y) (-sx + x, sy + y)
  where
    sx = width / 2
    sy = height / 2

defaultButtonImg :: Rectangle -> Picture -> Picture -> Point -> Picture
defaultButtonImg r@(Rectangle (x, y) width height thickness) prim sec p = translate x y (pictures [thickRectangle width height thickness color black,img])
  where
    (color,img) | rectangleHovered p r = (white,sec)
                | otherwise = (blue,prim)

defaultButton :: Rectangle -> Font -> String -> Point -> IO Picture
defaultButton r f s p = completeButton r f s p blue white

completeButton :: Rectangle -> Font -> String -> Point -> Color -> Color -> IO Picture
completeButton box font text mouse normal hover =
  renderButton
    box
    font
    text
    (if rectangleHovered mouse box
       then hover
       else normal)

renderButton :: Rectangle -> Font -> String -> Color -> IO Picture
renderButton (Rectangle (x, y) width height thickness) f s c = do
  text <- renderString (0, 0) f c s
  return (translate x y (pictures [thickRectangle width height thickness c black, text]))

thickRectangle :: Float -> Float -> Float -> Color -> Color -> Picture
thickRectangle w h t fg bg =
  let t2 = t / 2
   in pictures [Color fg (rectangleSolid (w + t2) (h + t2)), Color bg (rectangleSolid (w - t2) (h - t2))]

colorToV4 :: Color -> V4 Word8
colorToV4 c =
  let (r, g, b, a) = rgbaOfColor c
   in V4 (floor r * 255) (floor g * 255) (floor b * 255) (floor a * 255)

renderStringTopRight :: Point -> Font -> Color -> String -> IO Picture
renderStringTopRight _ _ _ "" = do
  return blank
renderStringTopRight (x, y) f c txt = do
  sections <- mapM (renderString' f c) (reverse $ lines txt)
  let width = foldr (\((w, _), _) mw -> max w mw) 0 sections
  let (height, imgs) = foldr (\((w, h), pic) (ch, pics) -> (ch + h, translate ((width - w) / 2) (-ch - (h / 2)) pic : pics)) (0, []) sections
  do return (translate (-width / 2 + x) y (pictures imgs))

renderStringTopLeft :: Point -> Font -> Color -> String -> IO Picture
renderStringTopLeft _ _ _ "" = do
  return blank
renderStringTopLeft (x, y) f c txt = do
  sections <- mapM (renderString' f c) (reverse $ lines txt)
  let width = foldr (\((w, _), _) mw -> max w mw) 0 sections
  let (height, imgs) = foldr (\((w, h), pic) (ch, pics) -> (ch + h, translate (-((width - w) / 2)) (-ch - (h / 2)) pic : pics)) (0, []) sections
  do return (translate (width / 2 + x) y (pictures imgs))

renderStringResize :: Point -> Font -> Color -> String -> Float -> Float -> IO Picture
renderStringResize _ _ _ "" _ _ = do
  return blank
renderStringResize (x, y) f c txt tw th = do
  ((width,height), pic) <- renderMultilineString f c txt
    -- make sure we properly center our rendered string about the center (x,y)
  do return $ translate x y (translate 0 (height / 2) $ resize width height tw th pic)

renderString :: Point -> Font -> Color -> String -> IO Picture
renderString _ _ _ "" = do
  return blank
renderString (x, y) f c txt = do
  sections <- mapM (renderString' f c) (reverse $ lines txt)
  ((_,height), pic) <- renderMultilineString f c txt
    -- make sure we properly center our rendered string about the center (x,y)
  do return $ translate x y (translate 0 (height / 2) pic)

renderMultilineString :: Font -> Color -> String -> IO ((Float,Float),Picture)
renderMultilineString f c s = do
  sections <- mapM (renderString' f c) (reverse $ lines s)
  let ((width,height), imgs) = foldr (\((w, h), pic) ((cw,ch), pics) -> ((max w cw,ch + h), translate 0 (-ch - (h / 2)) pic : pics)) ((0,0), []) sections
  return ((width,height),pictures imgs)

-- calculate string size for a given string s with font f
stringSize :: Font -> String -> IO (Float, Float)
stringSize f "" = do
  (_, h) <- stringSize f "f"
  return (0, h)
stringSize f s = do
  (w, h) <- size f (pack s)
  return (fromIntegral w :: Float, fromIntegral h :: Float)

renderString' :: Font -> Color -> String -> IO ((Float, Float), Picture)
renderString' f c s = do
  surface <- renderStringSurface f c s
  pic <- surfaceToPicture surface
  freeSurface surface -- if we don't free this surface we leak memory
  return pic

-- convert SDL2 surface to gloss BitMap picture
surfaceToPicture :: Surface -> IO ((Float, Float), Picture)
surfaceToPicture surface = do
  bmData <- copySDLToBitmap surface
  let (w, h) = bitmapSize bmData
  return ((fromIntegral w, fromIntegral h), Bitmap bmData)

copySDLToBitmap :: Surface -> IO BitmapData
copySDLToBitmap surface = do
  dims <- surfaceDimensions surface
  copy <- createRGBSurface (fromIntegral <$> dims) RGBA8888 -- create copy with same dimensions
  surfaceBlit surface Nothing copy Nothing -- copy pixels from surface to new surface in RGBA8888 format
  lockSurface copy -- acquire lock on the copy
  pixels <- surfacePixels copy -- get pointer to pixels
  let V2 (CInt w) (CInt h) = dims
  let cpSize = fromIntegral $ w * h * 4 -- width * height * 4 (space for R G B A)
  dest <- mallocForeignPtrBytes cpSize -- allocate bitmap
  withForeignPtr dest $ \destPtr -> copyBytes destPtr (castPtr pixels) cpSize -- copy pixel data to bitmap
  unlockSurface copy
  let bitmap = bitmapDataOfForeignPtr (fromIntegral w) (fromIntegral h) (BitmapFormat TopToBottom PxABGR) dest False -- convert foreign bitmap to gloss bitmap
  freeSurface copy
  return bitmap

renderStringSurface :: Font -> Color -> String -> IO Surface
renderStringSurface f c s = blended f (colorToV4 c) (pack s)
