module Assets where

import Data.List (isSuffixOf, sort)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import FontContainer (FontContainer (..), loadFont)
import Graphics.Gloss (Picture (Pictures), blank)
import Graphics.Gloss.Juicy (loadJuicyJPG, loadJuicyPNG)
import Map (WallSection, calculateIntersections, processWalls)
import SDL.Font (Font, initialize, load)
import Struct (Cell, LevelMap, readLevel)
import System.Directory (canonicalizePath, getDirectoryContents)
import System.FilePath (joinPath, takeBaseName, (</>))
import Text.Printf

type Anim = [Picture]

data PacManSprite = PacManSprite {
    up    :: Anim,
    down  :: Anim,
    left  :: Anim,
    right :: Anim
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


data Assets = Assets {
    pacFont     :: FontContainer,
    emuFont     :: FontContainer,
    pacSprite   :: PacManSprite,
    appleSprite :: Picture
  }

loadPacSprite :: String -> IO PacManSprite
loadPacSprite p = do
  up <- loadAnim (p </> "pacman-up")
  down <- loadAnim (p </> "pacman-down")
  left <- loadAnim (p </> "pacman-left")
  right <- loadAnim (p </> "pacman-right")
  return PacManSprite {
    up = up,
    down = down,
    left = left,
    right = right
  }

loadAssets :: String -> IO Assets
loadAssets p = do
    initialize
    pacFont <- loadFont (p </> "pacman.ttf")
    emuFont <- loadFont (p </> "emulogic.ttf")
    pacSprite <- loadPacSprite p
    appleSprite <- loadSprite (p </> "other/apple.png")
    return Assets {
        pacFont = pacFont,
        emuFont = emuFont,
        pacSprite = pacSprite,
        appleSprite = appleSprite
    }
