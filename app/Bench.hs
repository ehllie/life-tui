module Bench where

import Criterion.Main
import Game

runBench :: Int -> World -> IO ()
runBench generations world =
  defaultMain
    [ bgroup
        "GameOfLife"
        [ bench "Simple" $
            whnf (take generations) (iterate updateWorld world)
        ]
    ]
