{-# LANGUAGE OverloadedStrings #-}

module Html where

import Lucid
import Data.Text (Text)
import Data.Char (toUpper)
import qualified Data.Text as T


capitalize :: Text -> Text
capitalize t =
    case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> t

textInput name = do
  label_ [ for_ name ] (toHtml (capitalize name))
  input_ [ class_ "u-full-width", id_ name, type_ "text" ]

template title contents = do
  doctype_
  html_ $ do
    head_ $ do
      title_ renderedTitle
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]
      link_ [ rel_ "stylesheet", href_ "css/normalize.css" ]
      link_ [ rel_ "stylesheet", href_ "css/skeleton.css" ]
      link_ [ rel_ "stylesheet", href_ "css/bitsbacker.css" ]
      link_ [ rel_ "stylesheet", type_ "text/css"
            , href_ "//fonts.googleapis.com/css?family=Raleway:400,300,600"
            ]
    body_ $ do
      div_ [ class_ "header" ] $ do
        span_ [ class_ "logo" ] "bitsbacker"
      div_ [ class_ "container" ] $ do
        contents
  where
    renderedTitle =
      maybe "bitsbacker" ("bitsbacker | " <>) title
