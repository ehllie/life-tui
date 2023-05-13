module Cli (parse, Args (..)) where

import Options.Applicative (
  Parser,
  argument,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  str,
  switch,
  value,
 )

data Args = Args {aSpeed :: Double, aPattern :: String, aDynamic :: Bool}

parseArgs :: Parser Args
parseArgs = do
  aSpeed <- option auto (long "speed" <> short 's' <> value 1 <> help "World updates per second")
  aPattern <- argument str (metavar "PATTERN")
  aDynamic <- switch (long "dynamic" <> short 'd' <> help "Dynamic view")
  pure Args{aSpeed, aPattern, aDynamic}

parse :: IO Args
parse = execParser opts
 where
  opts =
    info
      (parseArgs <**> helper)
      (fullDesc <> progDesc "Run the game of life in the terminal" <> header "Game of Life TUI")
