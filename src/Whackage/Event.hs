module Whackage.Event where

import Data.Vector ((//))
import System.Random (randomR)

import Brick.Main
import Brick.Types
import Graphics.Vty.Input

import Whackage.Types

eventHandler :: AppState
             -> BrickEvent n CustomEvent
             -> EventM n (Next AppState)
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
eventHandler state (AppEvent CreateTarget) =
  continue $ createRandomTarget state
eventHandler state _ = continue state

createRandomTarget :: AppState -> AppState
createRandomTarget state@(AppState { gameGrid = oldGrid, randomGen = oldGen }) =
  state
    { gameGrid  = oldGrid // [(targetPos, Enemy)]
    , randomGen = nextGen
    }
  where
    (targetPos, nextGen) = randomR (0, 8) oldGen
