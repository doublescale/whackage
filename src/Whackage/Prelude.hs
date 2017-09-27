module Whackage.Prelude
  ( module Ex
  ) where

import Control.Applicative as Ex (Applicative(pure, (<*>)))
import Data.Bool as Ex (Bool(True, False), otherwise, bool)
import Data.Eq as Ex (Eq((==), (/=)))
import Data.Ord as Ex (Ord((<), (>), (<=), (>=)))
import Data.Functor as Ex (Functor(fmap), (<$>), void)
import Data.Function as Ex ((.), ($), (&), id, const)
import Data.List as Ex (repeat)
import Data.Maybe as Ex (Maybe(Nothing, Just))
import Data.Monoid as Ex ((<>))
import Data.String as Ex (String)
import GHC.Enum as Ex (Enum(succ, pred))
import GHC.Err as Ex (undefined)
import GHC.Num as Ex (Num((+), (-), (*), negate, abs))
import GHC.Int as Ex (Int)
import GHC.IO as Ex (IO)
import Lens.Micro.GHC as Ex
