module Whackage.Event where

import Whackage.Prelude

import Brick.Main
import Brick.Types
import Graphics.Vty.Input

import Whackage.Types

eventHandler :: AppState
             -> BrickEvent n CustomEvent
             -> EventM n (Next AppState)
eventHandler (InTitle titleState) (VtyEvent (EvKey _ _)) =
  continue . InGame . startGame $ titleState
eventHandler state@(InTitle _) _ = continue state
eventHandler (InGame gameState) event =
  fmap InGame <$> gameEventHandler gameState event

gameEventHandler :: GameState
                 -> BrickEvent n CustomEvent
                 -> EventM n (Next GameState)
gameEventHandler state (VtyEvent (EvKey KEsc _)) = halt state
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
