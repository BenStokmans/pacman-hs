module Assets where

import SDL.Font (Font, initialize, load)
import System.FilePath ((</>))
import FontContainer (FontContainer(..),loadFont)

data Assets = Assets { 
    pacFont :: FontContainer,
    emuFont :: FontContainer
  }

loadAssets :: String -> IO Assets
loadAssets p = do
    initialize
    pacFont <- loadFont (p </> "pacman.ttf")
    emuFont <- loadFont (p </> "emulogic.ttf")
    return Assets { 
        pacFont = pacFont,
        emuFont = emuFont
    }