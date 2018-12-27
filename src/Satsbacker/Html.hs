{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Html where

import Lucid
import Data.Text (Text)
import Data.Char (toUpper)
import Web.Scotty


import qualified Data.Text as T


capitalize :: Text -> Text
capitalize t =
    case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> t


textInput :: Text -> Html ()
textInput name = do
  label_ [ for_ name ] (toHtml (capitalize name))
  input_ [ class_ "u-full-width", id_ name, type_ "text" ]


template :: Maybe Text -> Html () -> Html ()
template title contents = do
  doctype_
  html_ $ do
    head_ $ do
      title_ (toHtml renderedTitle)
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]
      link_ [ rel_ "stylesheet", href_ "css/normalize.css" ]
      link_ [ rel_ "stylesheet", href_ "css/skeleton.css" ]
      link_ [ rel_ "stylesheet", href_ "css/satsbacker.css" ]
      link_ [ rel_ "stylesheet", type_ "text/css", href_ "css/font.css" ]
    body_ $ do
      div_ [ class_ "header" ] $ do
        span_ [ class_ "logo" ] "satsbacker"
      contents
  where
    renderedTitle =
      maybe "satsbacker" (<> " | satsbacker") title


content :: Html a -> ActionM ()
content = html . renderText
