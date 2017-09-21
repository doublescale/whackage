module Whackage.Types where

import Data.Vector (Vector)

import Brick.Main (App)

data AppState = AppState
  { gameGrid :: Vector Target
  }
data Target = NoTarget | Enemy
data CustomEvent = CreateTarget Int
type AppName = ()
type MyApp = App AppState CustomEvent AppName
