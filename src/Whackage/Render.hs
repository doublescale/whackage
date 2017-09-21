module Whackage.Render where

import Data.Vector ((!))

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center

import Whackage.Types

renderState :: AppState -> [Widget n]
renderState state = pure . center . renderGrid . gameGrid $ state
  where
    renderGrid grid =
      vBox
        [ hBox
          [ renderTarget $ grid ! (x + 3*y)
          | x <- [0,1,2] ]
        | y <- [2,1,0] ]
    renderTarget NoTarget = str "... " <=> str "    "
    renderTarget Enemy    = str "òuó " <=> str "    "
