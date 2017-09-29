module Whackage.Prelude
  ( module Ex
  ) where

import Control.Applicative as Ex (Applicative(pure, (<*>)))
import Data.Bool as Ex (Bool(True, False), otherwise, bool)
import Data.Eq as Ex (Eq((==), (/=)))
import Data.Ord as Ex (Ord((<), (>), (<=), (>=)))
import Data.Functor as Ex (Functor(fmap), (<$>), void)
import Data.Function as Ex ((.), ($), (&), id, const, flip)
import Data.List as Ex (repeat)
import Data.Maybe as Ex (Maybe(Nothing, Just))
import Data.Monoid as Ex ((<>))
import Data.String as Ex (String)
import Data.Tuple as Ex (fst, snd)
import GHC.Enum as Ex (Enum(succ, pred))
import GHC.Err as Ex (undefined)
import GHC.Int as Ex (Int)
import GHC.IO as Ex (IO)
import GHC.Num as Ex (Num((+), (-), (*), negate, abs))
import GHC.Show as Ex (Show(show))
