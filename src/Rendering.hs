module Rendering where

import Graphics.Gloss.SDL.Surface (CacheTexture (..), bitmapOfSurface, bitmapDataOfSurface)
import SDL.Video.Renderer (Surface)
import SDL.Font (Font, solid, blended)
import SDL.Vect (V4 (..))
import Text.Printf (printf)
import Data.Word (Word8)
import Graphics.Gloss( Color, Picture(Bitmap), rgbaOfColor, translate, scale)
import Data.Text (pack)

colorToV4 :: Color -> V4 Word8 
colorToV4 c = let (r,g,b,a) = rgbaOfColor c in V4 (floor r * 255) (floor g * 255) (floor b * 255) (floor a * 255)

renderStringTopLeft :: Font -> Color -> String -> IO Picture
renderStringTopLeft f c s = do 
    surface <- renderStringSurface f c s
    ((w,h),pic) <- bitmapOfSurface NoCache surface
    return (translate (w / 2) (h / (-2)) pic)

renderString :: Font -> Color -> String -> IO Picture
renderString f c s = do 
    surface <- renderStringSurface f c s
    (_,pic) <- bitmapOfSurface NoCache surface
    return pic

renderStringSurface :: Font -> Color -> String -> IO Surface
renderStringSurface f c s = blended f (colorToV4 c) (pack s)