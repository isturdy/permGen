module Main where

import           Criterion.Config
import           Criterion.Main

import qualified Array            as A
import qualified Map              as M

main = defaultMainWith myConfig (return ())  [
    mkGroup "map" M.showLongest
  , mkGroup "array" A.showLongest
  ]

myConfig = defaultConfig {
              -- Always GC between runs.
              cfgPerformGC = ljust True
            }

--mkGroup :: String ->
mkGroup nm f = bgroup nm [
    bench "5" $ nf f 5
  , bench "6" $ nf f 6
  , bench "7" $ nf f 7
  , bench "8" $ nf f 8
  ]
