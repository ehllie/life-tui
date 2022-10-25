module Main (main) where

import Const
import Criterion.Main
import Game

templates :: [String]
templates = ["acorn", "beacon", "glider", "gun", "infinite", "r-pentomino"]

simulationLengths :: [Int]
simulationLengths = [100, 1000, 10000, 50000]

main :: IO ()
main = do
  worlds <- mapM (readFile . templatePath) templates
  defaultMain
    [ bgroup
      template
      [ bench
        (show duration)
        $ whnf (take duration) [seq $ show state | state <- iterate updateWorld world]
      | duration <- simulationLengths
      ]
    | (template, world) <- zip templates (map readTemplate worlds)
    ]
