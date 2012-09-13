{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow

import Hakyll

main = hakyllWith config $ do
  -- twitter bootstrap
  match "bootstrap-img/*" $ do
    route idRoute
    compile copyFileCompiler

  -- assets
  match "javascripts/*" $ do
    route idRoute
    compile copyFileCompiler

  match "images/**/*" $ do
    route idRoute
    compile copyFileCompiler

  match "index.haml" $ do
    route $ setExtension "html"
    compile haml

  match "style.sass" $ do
    route $ setExtension "css"
    compile sass

sass :: Compiler Resource String
sass = getResourceString >>> unixFilter "sass" ["-s"] >>> arr compressCss

haml :: Compiler Resource String
haml = getResourceString >>> unixFilter "haml" ["-s", "-r", "coffee-filter"]

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "" }
