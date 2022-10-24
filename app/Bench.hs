module Bench where

import Criterion.Main
import Criterion.Main.Options (Mode)
import Game

runBench :: World -> Mode -> IO ()
runBench world mode =
  runMode
    mode
    [ bgroup
        "GameOfLife"
        [ bench "500" $ whnf (take 100) (iterate updateWorld world)
        , bench "1000" $ whnf (take 100) (iterate updateWorld world)
        , bench "5000" $ whnf (take 100) (iterate updateWorld world)
        , bench "10000" $ whnf (take 100) (iterate updateWorld world)
        , bench "50000" $ whnf (take 100) (iterate updateWorld world)
        ]
    ]
