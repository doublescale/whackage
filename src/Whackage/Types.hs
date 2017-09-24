module Whackage.Types where

import Whackage.Prelude

import Data.Array (Array, (//), listArray, bounds)
import System.Random (StdGen, randomR)

import Brick.Main (App)

import Whackage.Random ()

data Target = NoTarget | Enemy
data CustomEvent = CreateTarget
type NameTag = ()
type MyApp = App AppState CustomEvent NameTag
type GridPos = (Int, Int)

data AppState
  = InTitle TitleState
  | InGame GameState

data TitleState = TitleState { titleRandomGen :: StdGen }

data GameState = GameState
  { gameGrid  :: Array GridPos Target
  , randomGen :: StdGen
  }

startGame :: TitleState -> GameState
startGame (TitleState gen) =
  GameState
    { gameGrid  = listArray ((0,0), (2,2)) (repeat NoTarget)
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
    (targetPos, nextGen) = randomR (bounds oldGrid) oldGen
