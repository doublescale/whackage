module Whackage.Types where

import Whackage.Prelude
import Data.Array (Array, (!), (//), listArray, bounds)
import System.Random (StdGen, randomR)

import Brick.Main (App)

import Whackage.Random ()

data Target = NoTarget | Enemy deriving (Eq)
data CustomEvent = CreateTarget
type NameTag = ()
type MyApp = App AppState CustomEvent NameTag
type GridPos = (Int, Int)
type GameGrid = Array GridPos Target
type Score = Int

data AppState
  = InTitle
  | InGame GameState
  | InGameOver Score

data GameState = GameState
  { gameGrid    :: Array GridPos Target
  , playerHp    :: Int
  , playerScore :: Score
  , randomGen   :: StdGen
  }

emptyGrid :: GameGrid
emptyGrid = listArray ((0,0), (2,2)) (repeat NoTarget)

hitTarget :: GridPos -> GameState -> GameState
hitTarget targetPos state@GameState { gameGrid = grid } =
  if missed then
    state { playerHp = pred $ playerHp state }
  else
    state
      { gameGrid = grid // [(targetPos, NoTarget)]
      , playerScore = succ $ playerScore state
      }
  where missed = grid ! targetPos == NoTarget

makeRandomTarget :: GameState -> GameState
makeRandomTarget state@GameState { gameGrid = oldGrid, randomGen = oldGen } =
  state
    { gameGrid  = oldGrid // [(targetPos, Enemy)]
    , randomGen = nextGen
    }
  where
    (targetPos, nextGen) = randomR (bounds oldGrid) oldGen
