module Prompt where

import Assets (Assets(..))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import FontContainer (FontContainer(..))
import Graphics.Gloss (Picture(..), black, blank, blue, green, pictures, rectangleSolid, red, translate, white)
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), MouseButton(LeftButton), SpecialKey(..))
import Rendering (Rectangle(..), defaultButton, rectangleHovered, renderString, stringSize, thickRectangle)
import State (GameState(..), GlobalState(..), MenuRoute(..), Prompt(..), defaultPrompt)

emptyPrompt :: Prompt
emptyPrompt = Prompt {}

errorPrompt :: String -> Maybe Prompt
errorPrompt s =
  Just
    defaultPrompt
      { accentColor = red
      , promptText = s
      , showTextField = False
      , showConfirmButton = False
      , closeAction = \state _ -> state {route = StartMenu, prompt = Nothing}
      }

okayButton :: Prompt -> Rectangle
okayButton Prompt {showCloseButton = False} = Rectangle (0, -75) 100 50 10
okayButton _ = Rectangle (75, -75) 75 50 10

closeButton :: Prompt -> Rectangle
closeButton Prompt {showConfirmButton = False} = Rectangle (0, -75) 100 50 10
closeButton _ = Rectangle (-75, -75) 100 50 10

keysString :: GlobalState -> String
keysString s = intercalate "," $ map (\(Char k) -> [k]) $ filter f $ keys s
  where
    f :: Key -> Bool
    f k =
      case k of
        (Char _) -> True
        _ -> False

renderPrompt :: GlobalState -> Prompt -> IO Picture
renderPrompt s p = do
  let mEmu = m (emuFont (assets s))
  (w, h) <- stringSize mEmu value
  let blinker =
        translate
          (if value /= ""
             then w / 2 + 5
             else 0)
          0 $
        if blink p
          then Color white $ rectangleSolid 5 h
          else blank
  let underline = translate 0 (-h / 2 - 5) $ Color blue $ rectangleSolid w 5
  t <- renderString (0, 50) mEmu red text
  ks <- renderString (0, 25) mEmu green (keysString s)
  v <- renderString (0, 0) mEmu white value
  let mPos = mousePos s
  drawnOkayButton <- defaultButton (okayButton p) mEmu "OK" mPos
  drawnCloseButton <- defaultButton (closeButton p) mEmu "CLOSE" mPos
  let box = thickRectangle 350 250 10 (accentColor p) black
  return $
    pictures
      ([ box
       , t
       , ks
       , if showConfirmButton p
           then drawnOkayButton
           else blank
       , if showCloseButton p
           then drawnCloseButton
           else blank
       ] ++
       if showTextField p
         then [v, blinker, underline]
         else [blank])
  where
    text = promptText p
    value = promptValue p
    c = clock s

handleInputPrompt :: Event -> GlobalState -> GlobalState
handleInputPrompt (EventKey (SpecialKey KeyEsc) _ _ _) s@(GlobalState {prompt = Just p@(Prompt {promptValue = value, closeAction = close})}) =
  close s value
handleInputPrompt (EventKey (SpecialKey KeyBackspace) _ _ _) s@(GlobalState {prompt = Just p@(Prompt {promptValue = pv})}) =
  s
    { prompt =
        Just
          p
            { promptValue =
                if null pv
                  then ""
                  else init pv
            }
    }
handleInputPrompt (EventKey (SpecialKey KeySpace) _ _ _) s@(GlobalState {prompt = Just p@(Prompt {promptValue = pv})}) =
  s {prompt = Just p {promptValue = pv ++ " "}}
handleInputPrompt (EventKey (Char k) _ _ _) s@(GlobalState {prompt = Just p@(Prompt {promptValue = pv})}) =
  s {prompt = Just p {promptValue = pv ++ [k]}}
handleInputPrompt (EventKey (MouseButton LeftButton) _ _ _) s@(GlobalState {prompt = Just p@(Prompt { promptValue = value
                                                                                                    , confirmAction = confirm
                                                                                                    , closeAction = close
                                                                                                    })})
  | rectangleHovered (mousePos s) (okayButton p) = confirm s value
  | rectangleHovered (mousePos s) (closeButton p) = close s value
handleInputPrompt _ s = s

handleUpdatePrompt :: Float -> GlobalState -> Prompt -> IO GlobalState
handleUpdatePrompt f s p
  | not $ showTextField p = do return s
  | clock s - lastBlink p > blinkInterval p = do return s {prompt = Just p {blink = not $ blink p, lastBlink = clock s}}
  | otherwise = do return s
