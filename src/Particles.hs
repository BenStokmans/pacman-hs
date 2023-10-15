module Particles where
import State (GlobalState (..), GameState (..), Settings (windowSize))
import Graphics.Gloss ( Picture (..), circleSolid, translate, pictures, blue, rgbaOfColor, makeColor )
import Control.Monad.Random (MonadRandom (getRandomR), Rand, RandomGen)

drawParticles :: GlobalState -> Picture
drawParticles s = Color (makeColor 0 0 1 0.4) (pictures (map (\((x,y), r) -> translate x y (circleSolid r)) (particles s)))

animConst :: Float
animConst = 20

updateParticles :: Float -> GlobalState -> IO GlobalState
updateParticles f s | c-p < 0.4 = do return s { particles = map (\((xp,yp),r) -> ((xp,yp+(f*animConst)),r)) ps }
                    | otherwise = do
                      xp <- getRandomR (-x2+x2/12, x2-x2/12)
                      r <- getRandomR (2 :: Float, 8)
                      return s { gameState = gs {prevClock = c}, particles = ((xp,-y2-(r/2)),r):ps }
            where 
                gs = gameState s
                c = clock gs
                p = prevClock gs
                (x,y) = windowSize (settings s)
                (x2,y2) = (x/2,y/2)
                ps = filter (\((_,yp),r) -> yp-r/2 <= y2) (particles s)