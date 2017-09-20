module Main where

import Control.Monad
import qualified Data.Vector as Vector

import Brick
import Brick.Widgets.Center
import Graphics.Vty

main :: IO ()
main = void $ customMain (mkVty defaultConfig) Nothing myApp initState

data AppState = AppState
  { gameGrid :: Vector.Vector Target
  }
data Target = NoTarget | Enemy
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
initState = AppState
  { gameGrid = Vector.replicate 9 Enemy
  }

renderState :: AppState -> [Widget AppName]
renderState state = pure . center . renderGrid . gameGrid $ state
  where
    renderGrid grid =
      vBox
        [ hBox
          [ renderTarget $ grid Vector.! (x + 3*y)
          | x <- [0,1,2] ]
        | y <- [2,1,0] ]
    renderTarget NoTarget = str ". "
    renderTarget Enemy    = str "X "

eventHandler :: AppState
             -> BrickEvent AppName AppEvent
             -> EventM AppName (Next AppState)
eventHandler state (VtyEvent (EvKey k _)) = handleKey k
  where
    handleKey KEsc        = halt state
    handleKey (KChar '1') = hitTarget 0
    handleKey (KChar '2') = hitTarget 1
    handleKey (KChar '3') = hitTarget 2
    handleKey (KChar '4') = hitTarget 3
    handleKey (KChar '5') = hitTarget 4
    handleKey (KChar '6') = hitTarget 5
    handleKey (KChar '7') = hitTarget 6
    handleKey (KChar '8') = hitTarget 7
    handleKey (KChar '9') = hitTarget 8
    handleKey _           = continue state
    hitTarget i =
      continue $ state { gameGrid = gameGrid state Vector.// [(i, NoTarget)] }
eventHandler state _ = continue state
