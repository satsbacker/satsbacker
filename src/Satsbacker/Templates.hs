{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Templates where


import Data.List (isSuffixOf)
import Text.Mustache
import Data.Text (Text)


loadTemplates :: IO Template
loadTemplates =
  compileMustacheDir' (\s -> "html" `isSuffixOf` s
                          || ".txt" `isSuffixOf` s)
                      (PName "user")
                      "templates"



getTemplate :: Template -> Text -> Template
getTemplate templates pname =
    templates { templateActual = PName pname }


