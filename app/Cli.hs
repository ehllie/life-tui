module Cli (parse, Args (..), Command (..)) where

import Criterion.Main.Options (Mode, defaultConfig, parseWith)
import Options.Applicative

newtype Args = Args {cmd :: Command}
data Command
  = Run {fps :: Double, pattern :: String, static :: Bool}
  | Bench Mode

parseCmd :: Parser Command
parseCmd =
  hsubparser
    ( command "run" (info runCmd (progDesc "Run the game"))
        <> command "bench" (info runBench (progDesc "Run benchmarks"))
    )
 where
  runCmd = do
    fps <- option auto (long "fps" <> short 'f' <> value 1 <> help "Frames per second")
    pattern <- argument str (metavar "PATTERN")
    static <- switch (long "static" <> short 's' <> help "Static view")
    pure Run{fps, pattern, static}
  runBench = Bench <$> parseWith defaultConfig

parseArgs :: Parser Args
parseArgs = Args <$> parseCmd

parse :: IO Args
parse = execParser opts
 where
  opts =
    info
      (parseArgs <**> helper)
      (fullDesc <> progDesc "Run the game of life in the terminal" <> header "Game of Life TUI")
