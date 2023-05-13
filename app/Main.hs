module Main (main) where

import Cli (Args (..), parse)
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
import Terminal.Game (Event (..), GEnv (..), Game (..), Plane, playGame, stringPlane)

data State = State
  { mode :: Mode
  , world :: World
  , speed :: Double
  , updateCounter :: Double
  , centre :: Point
  , render :: Render
  }

data Mode
  = Paused
  | Running
  | Quit
  deriving (Eq)

data Render
  = Static
  | Dynamic

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

logicFunction :: GEnv -> State -> Event -> State
logicFunction _ State{mode, world, speed, updateCounter, centre, render} event =
  let keyIn = any ((== event) . KeyPress)
      realCounter = floor updateCounter
      fraction = updateCounter - fromIntegral realCounter
      newWorld = case mode of
        Running -> last $ take (realCounter + 1) (iterate updateWorld world)
        Paused | keyIn "l" -> updateWorld world
        _ -> world
      newMode
        | keyIn " " = case mode of
            Paused -> Running
            Running -> Paused
            _ -> mode
        | keyIn "q" = Quit
        | otherwise = mode
      newSpeed
        | keyIn "+" = speed * 1.2
        | keyIn "-" = speed / 1.2
        | otherwise = speed
      newCounter = case mode of
        Running -> fraction + speed
        _ -> updateCounter
      newCentre = case render of
        Static -> case event of
          KeyPress 'H' -> (fst centre - 1, snd centre)
          KeyPress 'L' -> (fst centre + 1, snd centre)
          KeyPress 'J' -> (fst centre, snd centre - 1)
          KeyPress 'K' -> (fst centre, snd centre + 1)
          _ -> centre
        Dynamic -> findCentre newWorld
      newRender = case event of
        KeyPress 't' -> case render of
          Dynamic -> Static
          Static -> Dynamic
        _ -> render
   in State
        { mode = newMode
        , world = newWorld
        , speed = newSpeed
        , updateCounter = newCounter
        , centre = newCentre
        , render = newRender
        }

drawFunction :: GEnv -> State -> Plane
drawFunction GEnv{eTermDims} State{world, centre} =
  let corner = centreToCorner centre eTermDims
      worldTab = toTable corner eTermDims world
   in stringPlane $ showTab worldTab

game :: State -> Game State
game state =
  Game
    { gLogicFunction = logicFunction
    , gInitState = state
    , gTPS = 10
    , gQuitFunction = \State{mode} -> mode == Quit
    , gDrawFunction = drawFunction
    }

main :: IO ()
main = do
  setTitle "Life-TUI"
  Args{fps, lifePattern, static} <- parse
  startWorld <- readTemplate <$> readFile (templatePath lifePattern)
  let initState =
        State
          { mode = Paused
          , world = startWorld
          , speed = 0.1
          , updateCounter = 0
          , centre = findCentre startWorld
          , render = if static then Static else Dynamic
          }
  playGame $ game initState
