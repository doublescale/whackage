module Whackage.Prelude
  ( module Ex
  ) where

import Control.Applicative as Ex (Applicative(pure, (<*>)))
import Data.Functor as Ex (Functor(fmap), (<$>), void)
import Data.Function as Ex ((.), ($), (&), id, const)
import Data.List as Ex (repeat)
import Data.Maybe as Ex (Maybe(Nothing, Just))
import GHC.Num as Ex (Num((+), (-), (*), negate, abs))
import GHC.Int as Ex (Int)
import GHC.IO as Ex (IO)
