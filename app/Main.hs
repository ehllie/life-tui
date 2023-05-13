module Main (main) where

import Cli (Args (..), parse)
import qualified Data.Map.Strict as Map
import Game (
  Cell (..),
  Dims,
  Point,
  World,
  findCentre,
  readTemplate,
  updateWorld,
 )
import Path (templatePath)
import System.Console.ANSI (setTitle)
import Terminal.Game (
  Event (..),
  GEnv (..),
  Game (..),
  Plane,
  blankPlaneFull,
  cell,
  playGame,
  vcat,
  word,
  (%),
 )
import Text.Printf (printf)

data GameState = GameState
  { mode :: Mode
  , history :: NonEmpty World
  , speed :: Double
  , updateCounter :: Double
  , centre :: Point
  , render :: Render
  , rollback :: Bool
  }

data Mode
  = Paused
  | Running
  | Reverse
  | Quit
  deriving (Eq)

data Render
  = Static
  | Dynamic
  deriving (Eq)

centreToCorner :: Point -> Dims -> Point
centreToCorner (x, y) (w, h) = (x - w `div` 2, y + (h - 1) `div` 2)

while :: Monad m => m Bool -> m () -> m ()
while cond action = do
  c <- cond
  when c $ do
    action
    while cond action

forwardWorld :: GameState -> GameState
forwardWorld gState@GameState{history} =
  gState
    { history = updated :| w : ws
    }
 where
  updated = updateWorld w
  (w :| ws) = history

backwardWorld :: GameState -> GameState
backwardWorld gState@GameState{history} =
  gState
    { history = w' :| ws'
    }
 where
  (w :| ws) = history
  (w', ws') = fromMaybe (w, []) $ uncons ws

logicFunction :: GEnv -> GameState -> Event -> GameState
logicFunction _ gState event =
  let isKey c = event == KeyPress c
   in flip execState gState do
        when (isKey 'q') $ modify \s -> s{mode = Quit}
        when (isKey '+') $ modify \s -> s{speed = speed s * 1.2}
        when (isKey '-') $ modify \s -> s{speed = speed s / 1.2}
        when (isKey 'r') $ modify \s -> s{rollback = not $ rollback s}
        when (isKey 't') $ modify \s ->
          s
            { render = case render s of
                Static -> Dynamic
                Dynamic -> Static
            }
        when (isKey ' ') $ modify \s ->
          s
            { mode = case mode s of
                Paused -> Running
                Running -> Paused
                _ -> mode s
            }

        mode <- gets mode
        rollback <- gets rollback
        when (mode == Running) do
          while (gets ((>= 1) . updateCounter)) do
            modify if rollback then backwardWorld else forwardWorld
            modify $ \s -> s{updateCounter = updateCounter s - 1}
          modify $ \s -> s{updateCounter = updateCounter s + speed s}

        when (mode == Paused) do
          when (isKey 'f') $ modify forwardWorld
          when (isKey 'b') $ modify backwardWorld

        render <- gets render
        when (render == Static) do
          centre <- gets centre
          when (isKey 'h') $ modify \s -> s{centre = (fst centre - 1, snd centre)}
          when (isKey 'l') $ modify \s -> s{centre = (fst centre + 1, snd centre)}
          when (isKey 'j') $ modify \s -> s{centre = (fst centre, snd centre - 1)}
          when (isKey 'k') $ modify \s -> s{centre = (fst centre, snd centre + 1)}
          when (isKey '0') $ modify \s -> s{centre = findCentre . last $ history s}

        when (render == Dynamic) do
          world <- gets $ head . history
          modify $ \s -> s{centre = findCentre world}

drawFunction :: GEnv -> GameState -> Plane
drawFunction gEnv@GEnv{eTermDims, eFPS} GameState{history, centre, mode, render, speed, rollback} =
  let (cCol, cRow) = centreToCorner centre eTermDims
      world = head history
      lifePlane = Map.foldlWithKey addCell (blankPlaneFull gEnv) world
      addCell plane (col, row) c =
        if c == Alive
          then plane & framePoint % cell 'X'
          else plane
       where
        -- ansi-terminal-game treats coordinates as (row, col),
        -- with (1,1) being in the top left corner
        -- but our world is (col, row) with (0, 0) in the bottom left corner
        framePoint = (cRow - row, col - cCol)
      uiPlane =
        vcat $
          [ word (printf "Alive cells: %d" . Map.size $ Map.filter (== Alive) world)
          , word (printf "Generation: %d" $ length history)
          , word (printf "Speed: %.2f" (speed * fromIntegral usedTPS))
          , word (printf "FPS: %d" eFPS)
          ]
            <> [word "Paused" | mode == Paused]
            <> [word "Rewinding" | mode == Running, rollback]
            <> [word "Camera locked" | render == Dynamic]
   in lifePlane
        & (1, 1) % uiPlane

usedTPS :: Integer
usedTPS = 15

game :: GameState -> Game GameState
game initState =
  Game
    { gLogicFunction = logicFunction
    , gInitState = initState
    , gTPS = usedTPS
    , gQuitFunction = \GameState{mode} -> mode == Quit
    , gDrawFunction = drawFunction
    }

main :: IO ()
main = do
  setTitle "Life-TUI"
  Args{aSpeed, aPattern, aDynamic} <- parse
  startWorld <- readTemplate <$> readFileText (templatePath aPattern)
  let initState =
        GameState
          { mode = Paused
          , history = startWorld :| []
          , speed = aSpeed / fromIntegral usedTPS
          , updateCounter = 0
          , centre = findCentre startWorld
          , render = if aDynamic then Dynamic else Static
          , rollback = False
          }
  playGame $ game initState
