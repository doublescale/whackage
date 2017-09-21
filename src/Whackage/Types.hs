module Whackage.Types where

import Data.Vector (Vector, (//))
import System.Random (StdGen, randomR)

import Brick.Main (App)

data Target = NoTarget | Enemy
data CustomEvent = CreateTarget
type NameTag = ()
type MyApp = App AppState CustomEvent NameTag
type GridPos = Int

data AppState = InGame GameState
data GameState = GameState
  { gameGrid  :: Vector Target
  , randomGen :: StdGen
  }

hitTarget :: GridPos -> GameState -> GameState
hitTarget targetPos state@(GameState { gameGrid = grid }) =
  state { gameGrid = grid // [(targetPos, NoTarget)] }

makeRandomTarget :: GameState -> GameState
makeRandomTarget state@(GameState { gameGrid = oldGrid, randomGen = oldGen }) =
  state
    { gameGrid  = oldGrid // [(targetPos, Enemy)]
    , randomGen = nextGen
    }
  where
    (targetPos, nextGen) = randomR (0, 8) oldGen
