{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Hakyll

main = hakyllWith config $ do
  return ()

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "" }
