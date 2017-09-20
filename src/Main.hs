module Main where

import Control.Monad (void)
import qualified Data.Vector as Vector

import Brick.AttrMap
import Brick.Main
import Graphics.Vty

import Whackage.Types
import Whackage.Render
import Whackage.Event

main :: IO ()
main = void $ customMain (mkVty defaultConfig) Nothing myApp initState

myApp :: MyApp
myApp = App
  { appDraw = renderState
  , appChooseCursor = showFirstCursor
  , appHandleEvent = eventHandler
  , appStartEvent = return
  , appAttrMap = \s -> attrMap defAttr []
  }

initState :: AppState
initState = AppState
  { gameGrid = Vector.replicate 9 Enemy
  }
