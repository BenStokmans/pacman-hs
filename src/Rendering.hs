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
import Graphics.Gloss.Rendering (BitmapData(..), BitmapFormat(..), PixelFormat(..), RowOrder(..), bitmapDataOfForeignPtr, bitmapSize)
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
import Struct (Cell(..), GridInfo, Vec2(..))
import Text.Printf (printf)

data Rectangle =
  Rectangle Point Float Float Float --Centre point, width, height, border thickness

cellSize :: GridInfo -> (Float, Float) --cell size in px
cellSize ((c, r), (w, h)) = (w / c, h / r)

gridToScreenPos :: GridInfo -> Vec2 -> Point -- get position screen from grid position
gridToScreenPos gi@(dim, (w, h)) (Vec2 x y) =
  let (cw, ch) = cellSize gi
   in (x * cw - (w / 2) + cw / 2, y * ch - (h / 2) + ch / 2)

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

renderString :: Point -> Font -> Color -> String -> IO Picture
renderString _ _ _ "" = do
  return blank
renderString (x, y) f c txt = do
  sections <- mapM (renderString' f c) (reverse $ lines txt)
  let (height, imgs) = foldr (\((_, h), pic) (ch, pics) -> (ch + h, translate 0 (-ch - (h / 2)) pic : pics)) (0, []) sections
  do return $ translate x y (translate 0 (height / 2) (pictures imgs))

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
  lockSurface copy -- aquire lock on the copy
  pixels <- surfacePixels copy -- get pointer to pixels
  let V2 (CInt w) (CInt h) = dims
  let cpSize = fromIntegral $ w * h * 4 -- width * height * 4 (space for R G B A)
  dest <- mallocForeignPtrBytes cpSize -- alloc bitmap
  withForeignPtr dest $ \destPtr -> copyBytes destPtr (castPtr pixels) cpSize -- copy pixel data to bitmap
  unlockSurface copy
  let bitmap = bitmapDataOfForeignPtr (fromIntegral w) (fromIntegral h) (BitmapFormat TopToBottom PxABGR) dest False -- convert foreign bitmap to gloss bitmap
  freeSurface copy
  return bitmap

renderStringSurface :: Font -> Color -> String -> IO Surface
renderStringSurface f c s = blended f (colorToV4 c) (pack s)
