module ConstQ (templatesDirQ) where

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.Environment
import System.FilePath

templatesDirQ :: Q Exp
templatesDirQ =
  let getCurrent = (</> "templates") <$> getCurrentDirectory
   in lift =<< runIO (getEnv "LIFE_TEMPLATE_DIR" <|> getCurrent)
