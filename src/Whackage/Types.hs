module Whackage.Types where

import Data.Vector (Vector)
import System.Random (StdGen)

import Brick.Main (App)

data AppState = AppState
  { gameGrid  :: Vector Target
  , randomGen :: StdGen
  }
data Target = NoTarget | Enemy
data CustomEvent = CreateTarget
type AppName = ()
type MyApp = App AppState CustomEvent AppName
