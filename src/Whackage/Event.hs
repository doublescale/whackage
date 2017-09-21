module Whackage.Event where

import Brick.Main
import Brick.Types
import Graphics.Vty.Input

import Whackage.Types

eventHandler :: AppState
             -> BrickEvent n CustomEvent
             -> EventM n (Next AppState)
eventHandler (InGame gameState) event =
  fmap InGame <$> gameEventHandler gameState event

gameEventHandler :: GameState
                 -> BrickEvent n CustomEvent
                 -> EventM n (Next GameState)
gameEventHandler state (VtyEvent (EvKey k _)) = handleKey k
  where
    handleKey KEsc        = halt state
    handleKey (KChar '1') = hit 0
    handleKey (KChar '2') = hit 1
    handleKey (KChar '3') = hit 2
    handleKey (KChar '4') = hit 3
    handleKey (KChar '5') = hit 4
    handleKey (KChar '6') = hit 5
    handleKey (KChar '7') = hit 6
    handleKey (KChar '8') = hit 7
    handleKey (KChar '9') = hit 8
    handleKey _           = continue state
    hit i = continue $ hitTarget i state
gameEventHandler state (AppEvent CreateTarget) =
  continue $ makeRandomTarget state
gameEventHandler state _ = continue state
