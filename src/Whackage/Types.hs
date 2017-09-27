{-# LANGUAGE TemplateHaskell #-}

module Whackage.Types where

import Whackage.Prelude
import Data.Array (Array, listArray, bounds)
import Lens.Micro.TH (makeLenses)
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
  { _gameGrid    :: Array GridPos Target
  , _playerHp    :: Int
  , _playerScore :: Score
  , _randomGen   :: StdGen
  }
makeLenses ''GameState

emptyGrid :: GameGrid
emptyGrid = listArray ((0,0), (2,2)) (repeat NoTarget)

hitTarget :: GridPos -> GameState -> GameState
hitTarget targetPos state =
  if missed then
    state & playerHp %~ pred
  else
    state
      & gameGrid . ix targetPos .~ NoTarget
      & playerScore %~ succ
  where missed = state ^? gameGrid . ix targetPos ^. non NoTarget == NoTarget

makeRandomTarget :: GameState -> GameState
makeRandomTarget state =
  state
    & gameGrid . ix targetPos .~ Enemy
    & randomGen .~ nextGen
  where
    (targetPos, nextGen) =
      randomR (bounds (state ^. gameGrid)) (state ^. randomGen)
