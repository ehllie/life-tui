module Game (Point, Cell, World, updateWorld, lookupWorld, newWorld, toTable, toTable') where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

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
aliveNeighbours world =
  length
    . Set.filter (\p -> lookupWorld world p == Alive)
    . neighbors

updateWorld :: World -> World
updateWorld prev = Map.mapWithKey reslove considered
 where
  reslove p = Maybe.fromMaybe (if aliveNeighbours prev p == 3 then Alive else Dead)
  preferJust l r = Maybe.listToMaybe $ Maybe.catMaybes [l, r]
  considered =
    Map.foldlWithKey
      (\acc p c -> Map.unionWith preferJust acc $ nextGen prev p c)
      Map.empty
      prev

nextGen :: World -> Point -> Cell -> Map.Map Point (Maybe Cell)
nextGen w p =
  let alive = aliveNeighbours w p
      self = Map.singleton p (if alive > 1 && alive < 4 then Just Alive else Just Dead)
      others = Map.fromSet (const Nothing) $ neighbors p
   in \case
        Dead -> if alive == 3 then Map.singleton p (Just Alive) else Map.empty
        Alive -> Map.union self others

newWorld :: [Point] -> World
newWorld = Map.fromList . map (,Alive)

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
  nonDead = Map.filter (/= Dead) m
  (minP, _) = Maybe.fromMaybe ((0, 0), Dead) $ Map.lookupMin nonDead
  (maxP, _) = Maybe.fromMaybe ((0, 0), Dead) $ Map.lookupMax nonDead
