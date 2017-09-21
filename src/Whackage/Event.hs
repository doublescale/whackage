module Whackage.Event where

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
    handleKey (KChar '1') = hitTarget 0
    handleKey (KChar '2') = hitTarget 1
    handleKey (KChar '3') = hitTarget 2
    handleKey (KChar '4') = hitTarget 3
    handleKey (KChar '5') = hitTarget 4
    handleKey (KChar '6') = hitTarget 5
    handleKey (KChar '7') = hitTarget 6
    handleKey (KChar '8') = hitTarget 7
    handleKey (KChar '9') = hitTarget 8
    handleKey _           = id
gameEventHandler state (AppEvent CreateTarget) =
  continue $ makeRandomTarget state
gameEventHandler state _ = continue state
