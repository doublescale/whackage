{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Whackage.Types where

import Whackage.Prelude
import Control.Monad.State (MonadState, state, runState, execState)
import Data.Array (Array, listArray, bounds)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import System.Random (StdGen, randomR)

import Brick.Main (App)

import Whackage.Random ()

data Target = NoTarget | Enemy deriving (Eq, Show)
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
  } deriving (Show)
makeLenses ''GameState

emptyGrid :: GameGrid
emptyGrid = listArray ((0,0), (2,2)) (repeat NoTarget)

hitTarget :: GridPos -> GameState -> GameState
hitTarget targetPos st =
  if missed then
    st & playerHp %~ pred
  else
    st & gameGrid . ix targetPos .~ NoTarget
       & playerScore %~ succ
  where missed = st ^? gameGrid . ix targetPos ^. non NoTarget == NoTarget

randomPos :: (MonadState GameState m) => m GridPos
randomPos = do
  limits <- use (gameGrid . to bounds)
  zoom' randomGen . state $ randomR limits
  where zoom' l = state . runState . zoom l

makeRandomTarget :: GameState -> GameState
makeRandomTarget = execState $ do
  targetPos <- randomPos
  gameGrid . ix targetPos .= Enemy
