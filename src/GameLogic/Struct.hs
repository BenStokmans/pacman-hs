module GameLogic.Struct where

data GhostType
  = Blinky
  | Pinky
  | Inky
  | Clyde
  deriving (Eq, Show)

ghosts :: [GhostType]
ghosts = [Blinky, Pinky, Inky, Clyde]