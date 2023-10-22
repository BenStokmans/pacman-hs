module Rendering where

import Data.Text (intercalate, pack)
import Data.Word (Word8)
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
import Graphics.Gloss.SDL.Surface (CacheTexture(..), bitmapDataOfSurface, bitmapOfSurface)
import SDL.Font (Font, blended)
import SDL.Vect (V4(..))
import SDL.Video.Renderer (Surface)
import Text.Printf (printf)

data Rectangle =
  Rectangle Point Float Float Float --Centre point, width, height, borderthickness

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
  surface <- blended f (V4 0 0 0 0) (pack s)
  (s, _) <- bitmapOfSurface NoCache surface
  return s

renderString' :: Font -> Color -> String -> IO ((Float, Float), Picture)
renderString' f c s = do
  surface <- renderStringSurface f c s
  bitmapOfSurface NoCache surface

renderStringSurface :: Font -> Color -> String -> IO Surface
renderStringSurface f c s = blended f (colorToV4 c) (pack s)
