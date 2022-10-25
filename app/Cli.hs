module Cli (parse, Args (..)) where

import Options.Applicative

data Args = Args {fps :: Double, pattern :: String, static :: Bool}

parseArgs :: Parser Args
parseArgs = do
  fps <- option auto (long "fps" <> short 'f' <> value 1 <> help "Frames per second")
  pattern <- argument str (metavar "PATTERN")
  static <- switch (long "static" <> short 's' <> help "Static view")
  pure Args{fps, pattern, static}

parse :: IO Args
parse = execParser opts
 where
  opts =
    info
      (parseArgs <**> helper)
      (fullDesc <> progDesc "Run the game of life in the terminal" <> header "Game of Life TUI")
