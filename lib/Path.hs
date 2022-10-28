{-# LANGUAGE TemplateHaskell #-}

module Path (templatesDir, templatePath) where

import System.FilePath ((<.>), (</>))

import System.Directory (getCurrentDirectory)
import TH (compileEnv)

templatesDir :: FilePath
templatesDir = $(compileEnv "LIFE_TEMPLATE_DIR" ((</> "templates") <$> getCurrentDirectory))

templatePath :: String -> FilePath
templatePath name = templatesDir </> name <.> "gol"
