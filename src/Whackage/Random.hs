{-# OPTIONS_GHC -fno-warn-orphans #-}

module Whackage.Random where

import System.Random (Random, randomR, random)

instance (Random a, Random b) => Random (a, b) where
  randomR ((a,x), (b,y)) g0 = ((c,z), g2)
    where
      (c, g1) = randomR (a, b) g0
      (z, g2) = randomR (x, y) g1
  random g0 = ((a,x), g2)
    where
      (a, g1) = random g0
      (x, g2) = random g1
