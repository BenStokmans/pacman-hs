module Assets where

import Struct (LevelMap,readLevel, Cell)
import Map (calculateIntersections, WallSection, processWalls)
import SDL.Font (Font, initialize, load)
import FontContainer (FontContainer(..),loadFont)
import Graphics.Gloss (Picture (Pictures), blank)
import Graphics.Gloss.Juicy (loadJuicyPNG, loadJuicyJPG)
import System.Directory (canonicalizePath, getDirectoryContents)
import System.FilePath (joinPath, takeBaseName, (</>))
import Data.List (isSuffixOf, sort)
import Data.Maybe (mapMaybe,catMaybes, fromMaybe)
import Text.Printf

type Anim = [Picture]

data PacManSprite = PacManSprite {
    up :: Anim,
    down :: Anim,
    left :: Anim,
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
    pacFont :: FontContainer,
    emuFont :: FontContainer,
    pacSprite :: PacManSprite,
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