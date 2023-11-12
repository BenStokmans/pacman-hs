module Views.LeaderBoardView where

import Graphics.Gloss ( blue, pictures, Picture, white )
import State
    ( GlobalState(history, assets, settings, mousePos, route, highScores),
      Settings(debugEnabled, debugSettings) )
import Graphics.Gloss.Interface.IO.Game
    (Key(MouseButton, SpecialKey),
      MouseButton(LeftButton),
      SpecialKey(KeyEsc),
      Event(EventKey) )
import Rendering
    ( Rectangle(..),
      rectangleHovered,
      defaultButton,
      renderStringResize, renderString )
import FontContainer (FontContainer(..))
import Assets (Assets(..))
import Views.StartMenu (drawParticles, updateParticles)
import Struct
import Data.Map (toList)
import Data.List (sortBy)

closeButton :: Rectangle
closeButton = Rectangle (0, -330) 420 50 10

renderLeaderBoardView :: GlobalState -> IO Picture
renderLeaderBoardView gs = do
  title <- renderStringResize (0, 250) (xxl (pacFont (assets gs))) blue "LEADERBOARD" 775 75
  let mEmu = m (emuFont (assets gs))
  let mPos = mousePos gs
  let sett = settings gs
  let debug = not $ debugEnabled sett
  let ds = debugSettings sett
  let scores = reverse (zip [0..] $ take 8 $ sortBy (\(_,s1) (_,s2) -> compare s2 s1) (toList $ highScores gs))
  let (_,names) = foldr (\(i,(name,score)) (y,pics) -> (y-60, renderString (0,y) mEmu white (show (i+1) ++ ". " ++ name ++ ": " ++ show score) : pics)) (180,[]) scores
  lBoardPictures <- sequence names

  drawnCloseButton <- defaultButton closeButton mEmu "Close" mPos

  return (pictures [drawParticles gs, title, pictures lBoardPictures, drawnCloseButton])

handleInputLeaderBoardView :: Event -> GlobalState -> IO GlobalState
handleInputLeaderBoardView (EventKey (SpecialKey KeyEsc) _ _ _) s = do
  return s {route = head (history s), history = tailNull (history s)}
handleInputLeaderBoardView (EventKey (MouseButton LeftButton) _ _ _) s
  | rectangleHovered mPos closeButton = do return s {route = head his, history = tailNull his}
  | not (debugEnabled sett) = do return s
  | otherwise = do return s
  where
    sett = settings s
    his = history s
    ds = debugSettings sett
    mPos = mousePos s
handleInputLeaderBoardView _ s = do
  return s

handleUpdateLeaderBoardView :: Float -> GlobalState -> IO GlobalState
handleUpdateLeaderBoardView = updateParticles
