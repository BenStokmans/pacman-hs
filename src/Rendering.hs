module Rendering where

import Graphics.Gloss.SDL.Surface (CacheTexture (..), bitmapOfSurface, bitmapDataOfSurface)
import SDL.Video.Renderer (Surface)
import SDL.Font (Font, solid, blended)
import SDL.Vect (V4 (..))
import Text.Printf (printf)
import Data.Word (Word8)
import Graphics.Gloss( Color, Picture(Bitmap, Color), rgbaOfColor, translate, scale, rectangleWire, rectangleSolid, pictures, black, Point)
import Data.Text (pack)
import Graphics.Gloss.Data.Point ( Point, pointInBox )

data Rectangle = Rectangle Point Float Float Float --Centre point, width, height, borderthickness

rectangleHovered :: Point -> Rectangle -> Bool
rectangleHovered mouse (Rectangle (x,y) width height _) = pointInBox mouse (sx+x,-sy+y) (-sx+x,sy+y)
    where
        sx = width/2
        sy = height/2

renderButton :: Rectangle -> Font -> Color -> String -> IO Picture
renderButton (Rectangle (x,y) width height thickness) f c s = do
    text <- renderString (0,0) f c s
    return (translate x y (pictures [r1,r2,text]))
    where 
        w2 = thickness/2
        r1 = Color c (rectangleSolid (width+w2) (height+w2))
        r2 = Color black (rectangleSolid (width-w2) (height-w2))

colorToV4 :: Color -> V4 Word8 
colorToV4 c = let (r,g,b,a) = rgbaOfColor c in V4 (floor r * 255) (floor g * 255) (floor b * 255) (floor a * 255)

renderStringTopLeft :: Font -> Color -> String -> IO Picture
renderStringTopLeft f c s = do 
    surface <- renderStringSurface f c s
    ((w,h),pic) <- bitmapOfSurface NoCache surface
    return (translate (w / 2) (h / (-2)) pic)

renderString :: Point -> Font -> Color -> String -> IO Picture
renderString (x,y) f c s = do 
    surface <- renderStringSurface f c s
    (_,pic) <- bitmapOfSurface NoCache surface
    return (translate x y pic)

renderStringSurface :: Font -> Color -> String -> IO Surface
renderStringSurface f c s = blended f (colorToV4 c) (pack s)