module Main where

import Whackage.Prelude
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)

import Brick.AttrMap
import Brick.BChan
import Brick.Main
import Graphics.Vty

import Whackage.Types
import Whackage.Render
import Whackage.Event

main :: IO ()
main = do
  chan <- newBChan 64
  void . forkIO . forever $ do
    -- TODO: Only send events when in-game.
    threadDelay 500000
    writeBChan chan CreateTarget
  void $ customMain (mkVty defaultConfig) (Just chan) myApp InTitle

myApp :: MyApp
myApp = App
  { appDraw = renderState
  , appChooseCursor = showFirstCursor
  , appHandleEvent = eventHandler
  , appStartEvent = pure
  , appAttrMap = const $ attrMap defAttr []
  }
