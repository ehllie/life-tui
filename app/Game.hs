module Game (
  Cell,
  Dims,
  Point,
  World,
  lookupWorld,
  newWorld,
  readTemplate,
  toTable',
  toTable,
  updateWorld,
  worldDims,
) where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

data Cell = Alive | Dead deriving (Eq)

instance Show Cell where
  show Alive = "X"
  show Dead = " "

type Point = (Integer, Integer)

type Dims = (Integer, Integer)

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

toTable :: Point -> Dims -> World -> [[Cell]]
toTable (aX, aY) (w, h) m =
  [ [ lookupWorld m (x, y)
    | x <- [aX .. aX + w - 1]
    ]
  | y <- [aY, aY - 1 .. aY - h - 1]
  ]

readTemplate :: String -> World
readTemplate s =
  newWorld $
    concatMap
      (\(line, y) -> fst $ foldl walkLine ([], (0, y)) (words line))
      (zip (lines s) [0, -1 ..])
 where
  walkLine (ps, (x, y)) =
    let next = (x + 1, y)
     in \case
          "x" -> ((x, y) : ps, next)
          "." -> (ps, next)
          _ -> error "Invalid character"

worldDims :: World -> (Point, Dims)
worldDims w =
  let (xMin, xMax, yMin, yMax) =
        Map.foldlWithKey
          (\(xMin, xMax, yMin, yMax) (x, y) _ -> (min xMin x, max xMax x, min yMin y, max yMax y))
          (0, 0, 0, 0)
          w
   in ( (ceiling $ fromIntegral (xMax + xMin) / 2, ceiling $ fromIntegral (yMax + yMin) / 2)
      , (xMax - xMin + 1, yMax - yMin + 1)
      )

toTable' :: World -> [[Cell]]
toTable' m = toTable (cX - w + r, cY - h + b) (w, h) m
 where
  ((cX, cY), (w, h)) = worldDims m
  (r, b) = (w `div` 2, h `div` 2)
