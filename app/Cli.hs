module Cli (parse, Args (..), Command (..)) where

import Options.Applicative

data Args = Args
  { cmd :: Command
  , pattern :: String
  }

data Command = Run {fps :: Double} | Bench

parseCmd :: Parser Command
parseCmd =
  hsubparser
    ( command "run" (info runCmd (progDesc "Run the game"))
        <> command "bench" (info (pure Bench) (progDesc "Run benchmarks"))
    )
 where
  runCmd = do
    fps <- option auto (long "fps" <> short 'f' <> value 1 <> help "Frames per second")
    pure Run{fps}

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
