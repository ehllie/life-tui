{-# LANGUAGE TemplateHaskell #-}

module Const (templatesDir, templatePath) where

import System.FilePath

import ConstQ

templatesDir :: FilePath
templatesDir = $(templatesDirQ)

templatePath :: String -> FilePath
templatePath name = templatesDir </> name <.> "gol"
