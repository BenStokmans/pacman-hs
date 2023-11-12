module Assets where

import Control.Concurrent (forkIO)
import Control.Monad.Fix (fix)
import Data.List (isSuffixOf, sort)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import FontContainer (FontContainer(..), loadFont)
import Graphics.Gloss (Picture(Pictures), blank)
import Graphics.Gloss.Juicy (loadJuicyJPG, loadJuicyPNG)
import SDL.Font (Font, initialize, load)
import qualified SDL.Image as Image
import qualified SDL.Mixer as Mixer
import SDL.Video.Renderer (freeSurface)
import System.Directory (canonicalizePath, getDirectoryContents)
import System.FilePath ((</>), joinPath, takeBaseName)

import qualified SDL

import Control.Monad (when)
import Data.String (fromString)
import Data.Text (pack)
import Data.Text.Foreign (withCString)
import Foreign.C (newCString)
import Rendering (surfaceToPicture)
import SDL.Raw (rwFromFile)

type Anim = [Picture]

data PacManSprite = PacManSprite
  { up :: Anim
  , down :: Anim
  , left :: Anim
  , right :: Anim
  }

loadSprite :: String -> IO Picture
loadSprite s = do
  spriteMaybe <- loadJuicyPNG s
  return (fromMaybe blank spriteMaybe)

loadAnim :: String -> IO Anim
loadAnim path = do
  files <- getDirectoryContents path
  let filtered = sort $ map (path </>) (filter (\f -> ".png" `isSuffixOf` f) files)
  framesMaybe <- mapM loadJuicyPNG filtered
  let frames = catMaybes framesMaybe
  return (frames ++ tail (init frames))

data Assets = Assets
  { pacFont :: FontContainer
  , emuFont :: FontContainer
  , pacSprite :: PacManSprite
  , blinkySprite :: Picture
  , pinkySprite :: Picture
  , inkySprite :: Picture
  , clydeSprite :: Picture
  , blueGhostSprite :: Picture
  , whiteGhostSprite :: Picture
  , gearIconBlue :: Picture
  , gearIconWhite :: Picture
  , chartIconBlue :: Picture
  , chartIconWhite :: Picture
  , cherrySprite :: Picture
  , strawBerrySprite :: Picture
  , orangeSprite :: Picture
  , appleSprite :: Picture
  , melonSprite :: Picture
  , galaxianSprite :: Picture
  , bellSprite :: Picture
  , keySprite :: Picture
  }

loadPacSprite :: String -> IO PacManSprite
loadPacSprite p = do
  up <- loadAnim (p </> "pacman-up")
  down <- loadAnim (p </> "pacman-down")
  left <- loadAnim (p </> "pacman-left")
  right <- loadAnim (p </> "pacman-right")
  return PacManSprite {up = up, down = down, left = left, right = right}

startMusic :: String -> IO ()
startMusic p = do
  SDL.initialize [SDL.InitAudio]
  Mixer.openAudio (Mixer.Audio 48000 Mixer.FormatS16_LSB Mixer.Stereo) 256
  sound <- Mixer.load (p </> "theme.wav")
  Mixer.playForever sound
  Mixer.setVolume 20 Mixer.AllChannels
  Mixer.pause Mixer.AllChannels --FIXME: move or something
  fix $ \loop -> do
    SDL.delay 50
    playing <- Mixer.playing Mixer.AllChannels
    when playing loop
  Mixer.free sound
  Mixer.closeAudio
  Mixer.quit
  SDL.quit

loadImage :: String -> IO Picture
loadImage s = do
  svgString <- readFile s
  surface <- Image.decode (fromString svgString)
  (_, pic) <- surfaceToPicture surface
  freeSurface surface
  return pic

loadAssets :: String -> IO Assets
loadAssets p = do
  forkIO (startMusic p) -- start music thread
  initialize
  pacFont <- loadFont (p </> "pacman.ttf")
  emuFont <- loadFont (p </> "emulogic.ttf")
  pacSprite <- loadPacSprite p
  appleSprite <- loadSprite (p </> "ghosts/apple.png")
  blinkySprite <- loadSprite (p </> "ghosts/blinky.png")
  pinkySprite <- loadSprite (p </> "ghosts/pinky.png")
  inkySprite <- loadSprite (p </> "ghosts/inky.png")
  clydeSprite <- loadSprite (p </> "ghosts/clyde.png")
  blueGhostSprite <- loadSprite (p </> "ghosts/blue_ghost.png")
  whiteGhostSprite <- loadSprite (p </> "ghosts/white_ghost.png")
  gearIconBlue <- loadImage (p </> "gear-solid-blue.svg")
  gearIconWhite <- loadImage (p </> "gear-solid-white.svg")
  chartIconBlue <- loadImage (p </> "chart-bar-solid-blue.svg")
  chartIconWhite <- loadImage (p </> "chart-bar-solid-white.svg")
  cherrySprite <- loadSprite (p </> "fruits/cherry.png")
  strawBerrySprite <- loadSprite (p </> "fruits/strawberry.png")
  orangeSprite <- loadSprite (p </> "fruits/orange.png")
  appleSprite <- loadSprite (p </> "fruits/apple.png")
  melonSprite <- loadSprite (p </> "fruits/melon.png")
  galaxianSprite <- loadSprite (p </> "fruits/galaxian.png")
  bellSprite <- loadSprite (p </> "fruits/bell.png")
  keySprite <- loadSprite (p </> "fruits/key.png")
  return
    Assets
      { pacFont = pacFont
      , emuFont = emuFont
      , pacSprite = pacSprite
      , blinkySprite = blinkySprite
      , pinkySprite = pinkySprite
      , inkySprite = inkySprite
      , clydeSprite = clydeSprite
      , blueGhostSprite = blueGhostSprite
      , whiteGhostSprite = whiteGhostSprite
      , gearIconBlue = gearIconBlue
      , gearIconWhite = gearIconWhite
      , chartIconBlue = chartIconBlue
      , chartIconWhite = chartIconWhite
      , cherrySprite = cherrySprite
      , strawBerrySprite = strawBerrySprite
      , orangeSprite = orangeSprite
      , appleSprite = appleSprite
      , melonSprite = melonSprite
      , galaxianSprite = galaxianSprite
      , bellSprite = bellSprite
      , keySprite = keySprite
      }
