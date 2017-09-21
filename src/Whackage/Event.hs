module Whackage.Event where

import Data.Vector ((//))

import Brick.Main
import Brick.Types
import Graphics.Vty.Input

import Whackage.Types

eventHandler :: AppState
             -> BrickEvent AppName CustomEvent
             -> EventM AppName (Next AppState)
eventHandler state (VtyEvent (EvKey k _)) = handleKey k
  where
    handleKey KEsc        = halt state
    handleKey (KChar '1') = hitTarget 0
    handleKey (KChar '2') = hitTarget 1
    handleKey (KChar '3') = hitTarget 2
    handleKey (KChar '4') = hitTarget 3
    handleKey (KChar '5') = hitTarget 4
    handleKey (KChar '6') = hitTarget 5
    handleKey (KChar '7') = hitTarget 6
    handleKey (KChar '8') = hitTarget 7
    handleKey (KChar '9') = hitTarget 8
    handleKey _           = continue state
    hitTarget i =
      continue $ state { gameGrid = gameGrid state // [(i, NoTarget)] }
eventHandler state _ = continue state
