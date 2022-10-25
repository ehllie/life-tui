module Main (main) where

import Cli
import Const
import Control.Concurrent (threadDelay)
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
  Args{fps, pattern, static} <- parse
  startWorld <- readTemplate <$> readFile (templatePath pattern)
  let draw = if static then staticTable $ findCentre startWorld else wholeTable
  mapM_ (display fps draw) (iterate updateWorld startWorld)
