module Bench (runBench) where

import Criterion.Main
import Criterion.Main.Options (Mode)
import Game

templates :: [String]
templates = ["acorn", "beacon", "glider", "gun", "infinite", "r-pentomino"]

simulationLengths :: [Int]
simulationLengths = [100, 1000, 10000, 50000]

runBench :: Mode -> IO ()
runBench mode = do
  worlds <- mapM (readFile . (++) "/home/ellie/Code/life-tui/templates/" . (++ ".gol")) templates
  runMode
    mode
    [ bgroup
      template
      [ bench
        (show duration)
        $ whnf (take duration) [seq $ show state | state <- iterate updateWorld world]
      | duration <- simulationLengths
      ]
    | (template, world) <- zip templates (map readTemplate worlds)
    ]
