module Whackage.Render where

import Whackage.Prelude
import Data.Array ((!), bounds)
import Lens.Micro
import Text.Printf (printf)

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center

import Whackage.Types

renderState :: AppState -> [Widget n]
renderState InTitle = renderTitle
renderState (InGame gameState) = renderGame gameState
renderState (InGameOver score) = renderGameOver score

renderTitle :: [Widget n]
renderTitle = pure . spacedCentered $
  [ "WHACKAGE!"
  , "Hit the NumPad buttons to whack the mean faces!"
  , "Press any key to start."
  ]

renderGame :: GameState -> [Widget n]
renderGame state = [renderStatusPane state, center $ renderGrid state]

renderStatusPane :: GameState -> Widget n
renderStatusPane state = hBox . fmap (vBox . fmap str) $
  [ ["Health:", "Score:"]
  , printf "%4d" <$> state ^.. (playerHp <> playerScore)
  ]

renderGrid :: GameState -> Widget n
renderGrid state =
  vBox
    [ hBox
      [ renderTarget $ grid ! (y, x)
      | x <- [x0..x1] ]
    | y <- [y0..y1] ]
  where
    grid = state ^. gameGrid
    renderTarget NoTarget = str "... " <=> str "    "
    renderTarget Enemy    = str "òuó " <=> str "    "
    ((y0,x0), (y1,x1)) = bounds grid

renderGameOver :: Score -> [Widget n]
renderGameOver score = pure . spacedCentered $
  [ "GAME OVER"
  , printf "You got %d point%s." score (bool "s" "" (score == 1))
  , "Press Escape to exit."
  ]

spacedCentered :: [String] -> Widget n
spacedCentered = center . vBox . fmap (hCenter . padBottom (Pad 1) . str)
