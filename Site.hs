{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative ((<$>))
import Data.Monoid
import Data.Maybe

import Hakyll

main = hakyll $ do
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

  -- page template
  match "layout.haml" $ compile hamlTemplateCompiler

  -- partials
  match "partials/*.html" $ compile getResourceString
  match "partials/*.haml" $ compile haml

  match "index.haml" $ do
    route $ setExtension "html"
    compile $ do
      let baseContext = staticContextWithFooter "partials/footer.haml"
      let timelineField = field "timeline" (\_ -> loadBody "partials/timeline.html")
      haml >>= applyAsTemplate timelineField >>= loadAndApplyTemplate "layout.haml" baseContext

  match "404.haml" $ do
    route $ setExtension "html"
    compile $ do
      haml >>= loadAndApplyTemplate "layout.haml" staticContextWithoutFooter

  match "credits.html" $ do
    route $ idRoute
    compile copyFileCompiler

  match "style*.sass" $ do
    route $ setExtension "css"
    compile sass

staticContext :: Maybe Identifier -> Context String
staticContext maybeFooter = mconcat [
    field "typekit" (\_ -> loadBody "partials/typekit.html")
  , field "analytics" (\_ -> loadBody "partials/google_analytics.html")
  , case maybeFooter of
      Nothing -> constField "footer" ""
      Just f -> field "footer" (\_ -> loadBody f)
  , defaultContext
  ]

staticContextWithFooter :: Identifier -> Context String
staticContextWithFooter = staticContext . Just

staticContextWithoutFooter :: Context String
staticContextWithoutFooter = staticContext Nothing

sass :: Compiler (Item String)
sass = getResourceString >>= withItemBody (unixFilter "sass" ["-s"]) >>= return . fmap compressCss

haml :: Compiler (Item String)
haml = getResourceString >>= withItemBody (unixFilter "haml" ["-s", "-r", "coffee-filter", "-f", "html5"])

hamlTemplateCompiler :: Compiler (Item Template)
hamlTemplateCompiler = cached "Hakyll.Web.Template.templateCompiler" $ do
    item <- haml
    return $ fmap readTemplate item
