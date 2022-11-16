module Main (main) where

import Brick (Widget, simpleMain)
import Brick.Widgets.Core (str)
import Brick.Widgets.Table (
  columnBorders,
  renderTable,
  rowBorders,
  surroundingBorder,
  table,
 )
import Cli (Args (Args, brick, fps, pattern, static), parse)
import Control.Concurrent (threadDelay)
import Game (
  Cell,
  Dims,
  Point,
  World,
  findCentre,
  lookupWorld,
  readTemplate,
  updateWorld,
 )
import Path (templatePath)
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
  | y <- [aY, aY - 1 .. aY - h + 1]
  ]

centreToCorner :: Point -> Dims -> Point
centreToCorner (x, y) (w, h) = (x - w `div` 2, y + (h - 1) `div` 2)

wholeTable :: Dims -> World -> [[Cell]]
wholeTable dims world =
  toTable
    (centreToCorner (findCentre world) dims)
    dims
    world

staticTable :: Point -> Dims -> World -> [[Cell]]
staticTable anchor dims = toTable (centreToCorner anchor dims) dims

display :: Double -> (Dims -> World -> [[Cell]]) -> World -> IO ()
display fps draw world = do
  setCursorPosition 0 0
  (h, w) <- (\(Just (x, y)) -> return (fromIntegral x, fromIntegral y)) =<< getTerminalSize
  putStr $ showTab $ draw (w, h) world
  threadDelay (1000 * floor (1000 / fps))

main :: IO ()
main = do
  setTitle "Life-TUI"
  Args{fps, pattern, static, brick} <- parse
  startWorld <- readTemplate <$> readFile (templatePath pattern)
  if brick
    then do
      let ui :: Widget ()
          ui =
            renderTable
              . surroundingBorder False
              . rowBorders False
              . columnBorders False
              . table
              . map (map (str . show))
              $ wholeTable (15, 15) startWorld
      simpleMain ui
    else do
      let draw = if static then staticTable $ findCentre startWorld else wholeTable
      mapM_ (display fps draw) (iterate updateWorld startWorld)
