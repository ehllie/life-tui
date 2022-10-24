module Cli (parse, Args (..), Command (..)) where

import Criterion.Main.Options (Mode, defaultConfig, parseWith)
import Options.Applicative

data Args = Args {cmd :: Command}
data Command
  = Run {fps :: Double, pattern :: String}
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
    pure Run{fps, pattern}
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
