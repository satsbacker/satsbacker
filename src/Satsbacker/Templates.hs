{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Templates where


import Data.List (isSuffixOf)
import Text.Mustache

loadTemplates :: IO Template
loadTemplates = do
  compileMustacheDir' (isSuffixOf ".html")
                      (PName "user")
                      "templates"


