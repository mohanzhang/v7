{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow

import Hakyll

main = hakyllWith config $ do
  -- font awesome
  match "fontawesome-font/*" $ do
    route $ gsubRoute "fontawesome-font/" (const "font/")
    compile copyFileCompiler

  match "fontawesome-css/*" $ do
    route $ gsubRoute "fontawesome-css/" (const "css/")
    compile copyFileCompiler

  -- assets
  match "javascripts/*" $ route idRoute >> compile copyFileCompiler
  match "images/*" $ route idRoute >> compile copyFileCompiler
  match "images/**/*" $ route idRoute >> compile copyFileCompiler
  match "fonts/*" $ route idRoute >> compile copyFileCompiler

  -- partials
  match "partials/*.haml" $ compile haml
  match "partials/*.html" $ compile getResourceString

  match "index.haml" $ do
    route $ setExtension "html"
    compile $ haml
      >>> arr fromBody
      >>> requireA "partials/google_analytics.html" (setFieldA "analytics" returnA)
      >>> requireA "partials/typekit.html" (setFieldA "typekit" returnA)
      >>> requireA "partials/footer.haml" (setFieldA "footer" returnA)
      >>> applyTemplateCompiler "layout.haml"

  match "layout.haml" $ compile $ haml >>> arr readTemplate

  match "credits.html" $ do
    route $ idRoute
    compile copyFileCompiler

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
