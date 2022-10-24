module Main (main) where

import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.String.Interpolate
import System.Console.ANSI (clearScreen, setCursorPosition, setTitle)

data Cell = Alive | Dead deriving (Eq)

instance Show Cell where
  show Alive = "X"
  show Dead = " "

type Point = (Int, Int)

type World = (Map.Map Point Cell)

neighbors :: Point -> Set.Set Point
neighbors (x, y) =
  Set.fromAscList
    [ (x + dx, y + dy)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , dx /= 0 || dy /= 0
    ]

lookupWorld :: World -> Point -> Cell
lookupWorld = flip $ Map.findWithDefault Dead

aliveNeighbours :: World -> Point -> Int
aliveNeighbours world = length . Set.filter (\p -> lookupWorld world p == Alive) . neighbors

updateWorld :: World -> World
updateWorld prev = Map.mapWithKey reslove considered
 where
  alive = aliveNeighbours prev
  reslove p = Maybe.fromMaybe (if alive p == 3 then Alive else Dead)
  considered =
    Map.foldlWithKey
      (\acc p c -> Map.unionWith preferJust acc $ nextGen p c)
      Map.empty
      prev
   where
    preferJust l r = Maybe.listToMaybe $ Maybe.catMaybes [l, r]
    nextGen p Dead = if alive p == 3 then Map.singleton p (Just Alive) else Map.empty
    nextGen p Alive = Map.union self others
     where
      others = Map.fromSet (const Nothing) $ neighbors p
      self = Map.singleton p (if alive p > 1 && alive p < 4 then Just Alive else Just Dead)

toTable :: Point -> Point -> World -> [[Cell]]
toTable (minX, minY) (maxX, maxY) m =
  [ [ lookupWorld m (x, y)
    | x <- [minX .. maxX]
    ]
  | y <- [maxY, maxY - 1 .. minY]
  ]

toTable' :: World -> [[Cell]]
toTable' m = toTable minP maxP m
 where
  (minP, _) = Map.findMin m
  (maxP, _) = Map.findMax m

showTab :: Show a => [[a]] -> String
showTab tab = unlines $ map (unwords . map pad) shown
 where
  shown = map (map show) tab
  maxLen = maximum $ map length $ concat shown
  pad s = s ++ replicate (maxLen - length s) ' '

initState :: World
initState =
  Map.fromList
    . map
      (,Alive)
    $ [ (1, 1)
      , (1, 0)
      , (0, 1)
      , (-2, -2)
      , (-2, -1)
      , (-1, -2)
      ]

fps :: Double
fps = 0.5

display :: World -> IO ()
display world = do
  clearScreen
  setCursorPosition 0 0
  putStrLn $ showTab $ toTable' world
  threadDelay (1000 * floor (1000 / fps))

main :: IO ()
main = setTitle "Life-TUI" >> mapM_ display (iterate updateWorld initState)
