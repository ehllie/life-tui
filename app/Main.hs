module Main (main) where

import Bench
import Cli
import Control.Concurrent (threadDelay)
import Data.String.Interpolate (i)
import Game
import System.Console.ANSI (getTerminalSize, setCursorPosition, setTitle)

showTab :: Show a => [[a]] -> String
showTab tab = unlines $ map (concatMap pad) shown
 where
  shown = map (map show) tab
  maxLen = maximum $ map length $ concat shown
  pad s = s ++ replicate (maxLen - length s) ' '

toTable :: Point -> Dims -> World -> [[Cell]]
toTable (aX, aY) (w, h) m =
  [ [ lookupWorld m (x, y)
    | x <- [aX .. aX + w - 1]
    ]
  | y <- [aY, aY - 1 .. aY - h - 1]
  ]

wholeTable :: Dims -> World -> [[Cell]]
wholeTable (w, h) m = toTable anchor (w, h) m
 where
  (cX, cY) = findCentre m
  anchor = (cX - (w `div` 2), cY + h - (h `div` 2))

staticTable :: Point -> Dims -> World -> [[Cell]]
staticTable (cX, cY) (w, h) = toTable anchor (w, h)
 where
  anchor = (cX - (w `div` 2), cY + h - (h `div` 2))

display :: Double -> (Dims -> World -> [[Cell]]) -> World -> IO ()
display fps draw world = do
  setCursorPosition 0 0
  (h, w) <- (\(Just (x, y)) -> return (fromIntegral x, fromIntegral y)) =<< getTerminalSize
  putStr $ showTab $ draw (w, h) world
  threadDelay (1000 * floor (1000 / fps))

main :: IO ()
main = do
  setTitle "Life-TUI"
  Args{cmd} <- parse
  case cmd of
    Run{fps, pattern, static} -> do
      startWorld <- readTemplate <$> readFile [i|/home/ellie/Code/life-tui/templates/#{pattern}.gol|]
      let draw = if static then staticTable $ findCentre startWorld else wholeTable
      mapM_ (display fps draw) (iterate updateWorld startWorld)
    (Bench mode) -> runBench mode
