module Main (main) where

import Cli
import Control.Concurrent (threadDelay)
import Game
import System.Console.ANSI (clearScreen, setCursorPosition, setTitle)

showTab :: Show a => [[a]] -> String
showTab tab = unlines $ map (unwords . map pad) shown
 where
  shown = map (map show) tab
  maxLen = maximum $ map length $ concat shown
  pad s = s ++ replicate (maxLen - length s) ' '

initState :: World
initState =
  newWorld
    [ (1, 1)
    , (1, 0)
    , (0, 1)
    , (-2, -2)
    , (-2, -1)
    , (-1, -2)
    ]

-- fps :: Double
-- fps = 0.5

display :: Double -> World -> IO ()
display fps world = do
  clearScreen
  setCursorPosition 0 0
  putStrLn $ showTab $ toTable' world
  threadDelay (1000 * floor (1000 / fps))

main :: IO ()
-- main = setTitle "Life-TUI" >> mapM_ display (iterate updateWorld initState)
main = do
  setTitle "Life-TUI"
  args <- parse
  case cmd args of
    Run{fps} -> mapM_ (display fps) (iterate updateWorld initState)
    Bench -> putStrLn "Not implemented"
