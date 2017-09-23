module Whackage.Types where

import Whackage.Prelude

import Data.Vector (Vector, (//), replicate)
import System.Random (StdGen, randomR)

import Brick.Main (App)

data Target = NoTarget | Enemy
data CustomEvent = CreateTarget
type NameTag = ()
type MyApp = App AppState CustomEvent NameTag
type GridPos = Int

data AppState
  = InTitle TitleState
  | InGame GameState

data TitleState = TitleState { titleRandomGen :: StdGen }

data GameState = GameState
  { gameGrid  :: Vector Target
  , randomGen :: StdGen
  }

startGame :: TitleState -> GameState
startGame (TitleState gen) =
  GameState
    { gameGrid  = replicate 9 NoTarget
    , randomGen = gen
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
