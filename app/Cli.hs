module Cli (parse, Args (..), Command (..)) where

import Options.Applicative

data Args = Args
  { cmd :: Command
  , pattern :: String
  }

data Command
  = Run {fps :: Double}
  | Bench {generations :: Int}

parseCmd :: Parser Command
parseCmd =
  hsubparser
    ( command "run" (info runCmd (progDesc "Run the game"))
        <> command "bench" (info runBench (progDesc "Run benchmarks"))
    )
 where
  runCmd = do
    fps <- option auto (long "fps" <> short 'f' <> value 1 <> help "Frames per second")
    pure Run{fps}
  runBench = do
    generations <- option auto (long "generations" <> short 'g' <> value 100 <> help "Generations to run")
    pure Bench{generations}

parseArgs :: Parser Args
parseArgs = do
  cmd <- parseCmd
  pattern <- argument str (metavar "PATTERN")
  pure Args{cmd, pattern}

parse :: IO Args
parse = execParser opts
 where
  opts =
    info
      (parseArgs <**> helper)
      (fullDesc <> progDesc "Run the game of life in the terminal" <> header "Game of Life TUI")
