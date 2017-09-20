module Whackage.Types where

import Data.Vector (Vector)

import Brick.Main (App)

data AppState = AppState
  { gameGrid :: Vector Target
  }
data Target = NoTarget | Enemy
type AppEvent = ()
type AppName = ()
type MyApp = App AppState AppEvent AppName
