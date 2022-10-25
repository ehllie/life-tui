module ConstQ (templatesDirQ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.Environment
import System.FilePath

templatesDirQ :: Q Exp
templatesDirQ = do
  path <- runIO $ do
    env <- lookupEnv "LIFE_TEMPLATE_DIR"
    case env of
      Just dir -> return dir
      Nothing -> do
        current <- getCurrentDirectory
        return $ current </> "templates"
  lift path
