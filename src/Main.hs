module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forM_)
import qualified Data.Vector as Vector
import System.Random

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
  randomGen <- getStdGen
  void . forkIO . forM_ (randomRs (0, 8) randomGen) $ \targetPos -> do
    threadDelay 500000
    writeBChan chan (CreateTarget targetPos)
  void $ customMain (mkVty defaultConfig) (Just chan) myApp initState

myApp :: MyApp
myApp = App
  { appDraw = renderState
  , appChooseCursor = showFirstCursor
  , appHandleEvent = eventHandler
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr []
  }

initState :: AppState
initState = AppState
  { gameGrid = Vector.replicate 9 NoTarget
  }
