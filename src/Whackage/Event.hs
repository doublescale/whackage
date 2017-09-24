module Whackage.Event where

import Whackage.Prelude
import Control.Monad.IO.Class (liftIO)
import System.Random (getStdGen)

import Brick.Main
import Brick.Types
import Graphics.Vty.Input

import Whackage.Types

eventHandler :: AppState
             -> BrickEvent n CustomEvent
             -> EventM n (Next AppState)
eventHandler state (VtyEvent (EvKey KEsc _)) = halt state
eventHandler InTitle event = titleEventHandler event
eventHandler (InGame gameState) event =
  fmap nextState <$> gameEventHandler gameState event
    where nextState state@GameState { playerHp = hp, playerScore = score }
            | hp <= 0   = InGameOver score
            | otherwise = InGame state
eventHandler (InGameOver score) event =
  fmap InGameOver <$> gameOverEventHandler score event

titleEventHandler :: BrickEvent n e -> EventM n (Next AppState)
titleEventHandler (VtyEvent (EvKey _ _)) = do
  gen <- liftIO getStdGen
  continue . InGame $
    GameState
      { gameGrid    = emptyGrid
      , playerHp    = 5
      , playerScore = 0
      , randomGen   = gen
      }
titleEventHandler _ = continue InTitle

gameEventHandler :: GameState
                 -> BrickEvent n CustomEvent
                 -> EventM n (Next GameState)
gameEventHandler state (VtyEvent (EvKey k [])) = continue $ handleKey k state
  where
    handleKey (KChar '1') = hitTarget (2,0)
    handleKey (KChar '2') = hitTarget (2,1)
    handleKey (KChar '3') = hitTarget (2,2)
    handleKey (KChar '4') = hitTarget (1,0)
    handleKey (KChar '5') = hitTarget (1,1)
    handleKey (KChar '6') = hitTarget (1,2)
    handleKey (KChar '7') = hitTarget (0,0)
    handleKey (KChar '8') = hitTarget (0,1)
    handleKey (KChar '9') = hitTarget (0,2)
    handleKey _           = id
gameEventHandler state (AppEvent CreateTarget) =
  continue $ makeRandomTarget state
gameEventHandler state _ = continue state

gameOverEventHandler :: Score
                     -> BrickEvent n e
                     -> EventM n (Next Score)
gameOverEventHandler score _ = continue score
