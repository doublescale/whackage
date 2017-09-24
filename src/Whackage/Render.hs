module Whackage.Render where

import Whackage.Prelude

import Data.Array ((!), bounds)

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center

import Whackage.Types

renderState :: AppState -> [Widget n]
renderState (InTitle _) = renderTitle
renderState (InGame gameState) = renderGame gameState

renderTitle :: [Widget n]
renderTitle = pure . vCenter $
  line "WHACKAGE!" <=> padTop (Pad 1) (line "Press any key to start.")
  where line = hCenter . txt

renderGame :: GameState -> [Widget n]
renderGame state = pure . center . renderGrid . gameGrid $ state
  where
    renderGrid grid = let ((y0,x0), (y1,x1)) = bounds grid in
      vBox
        [ hBox
          [ renderTarget $ grid ! (y, x)
          | x <- [x0..x1] ]
        | y <- [y0..y1] ]
    renderTarget NoTarget = txt "... " <=> txt "    "
    renderTarget Enemy    = txt "òuó " <=> txt "    "
