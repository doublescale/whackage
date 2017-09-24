module Whackage.Render where

import Whackage.Prelude
import Data.Array ((!), bounds)
import Data.Text (Text, pack)
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
renderTitle = pure . spacedCentered $ ["WHACKAGE!", "Press any key to start."]

renderGame :: GameState -> [Widget n]
renderGame state = [renderStatusPane state, center $ renderGrid state]

renderStatusPane :: GameState -> Widget n
renderStatusPane state = hBox . fmap (vBox . fmap txt) $
  [ ["Health:", "Score:"]
  , pack . printf "%4d" <$> [playerHp state, playerScore state]
  ]

renderGrid :: GameState -> Widget n
renderGrid state =
  vBox
    [ hBox
      [ renderTarget $ grid ! (y, x)
      | x <- [x0..x1] ]
    | y <- [y0..y1] ]
  where
    grid = gameGrid state
    renderTarget NoTarget = txt "... " <=> txt "    "
    renderTarget Enemy    = txt "òuó " <=> txt "    "
    ((y0,x0), (y1,x1)) = bounds grid

renderGameOver :: Score -> [Widget n]
renderGameOver score = pure . spacedCentered $
  [ "Game Over" , pack $ printf "You got %d points." score ]

spacedCentered :: [Text] -> Widget n
spacedCentered = center . vBox . fmap (hCenter . padBottom (Pad 1) . txt)
