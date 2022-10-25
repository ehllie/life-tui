module Main (main) where

import Bench
import Cli
import Control.Concurrent (threadDelay)
import Data.String.Interpolate (i)
import Game
import System.Console.ANSI (clearScreen, setCursorPosition, setTitle)

showTab :: Show a => [[a]] -> String
showTab tab = unlines $ map (unwords . map pad) shown
 where
  shown = map (map show) tab
  maxLen = maximum $ map length $ concat shown
  pad s = s ++ replicate (maxLen - length s) ' '

display :: Double -> World -> IO ()
display fps world = do
  clearScreen
  setCursorPosition 0 0
  putStrLn $ showTab $ toTable' world
  threadDelay (1000 * floor (1000 / fps))

main :: IO ()
main = do
  setTitle "Life-TUI"
  Args{cmd} <- parse
  case cmd of
    Run{fps, pattern} -> do
      content <- readFile [i|/home/ellie/Code/life-tui/templates/#{pattern}.gol|]
      mapM_ (display fps) (iterate updateWorld $ readTemplate content)
    (Bench mode) -> runBench mode
