module TH (compileEnv) where

import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift (lift))
import System.Environment (getEnv)
import Prelude hiding (lift)

compileEnv :: String -> IO String -> Q Exp
compileEnv var fallback = lift =<< runIO (getEnv var <|> fallback)
