module Main where

import Control.Monad

import Brick
import Brick.Widgets.Center
import Graphics.Vty

main :: IO ()
main = void $ customMain (mkVty defaultConfig) Nothing myApp initState

type AppState = ()
type AppEvent = ()
type AppName = ()
type MyApp = App AppState AppEvent AppName

myApp :: MyApp
myApp = App
  { appDraw = renderState
  , appChooseCursor = showFirstCursor
  , appHandleEvent = eventHandler
  , appStartEvent = return
  , appAttrMap = \s -> attrMap defAttr []
  }

initState :: AppState
initState = ()

renderState :: AppState -> [Widget AppName]
renderState state = pure . center . str $ "Whack!"

eventHandler :: AppState
             -> BrickEvent AppName AppEvent
             -> EventM AppName (Next AppState)
eventHandler state (VtyEvent e) | isQuitEvent e = halt state
eventHandler state (AppEvent e) = continue e
eventHandler state _ = continue $ state

isQuitEvent :: Event -> Bool
isQuitEvent (EvKey KEsc _) = True
isQuitEvent _ = False
