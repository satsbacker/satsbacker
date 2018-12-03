{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import System.Environment
import Lucid
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

getPort :: IO Int
getPort = do
  mstrport <- lookupEnv "PORT"
  return (fromMaybe 8002 (mstrport >>= readMaybe))

template title contents = do
  doctype_
  html_ $ do
    head_ $ do
      title_ renderedTitle
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]
      link_ [ rel_ "stylesheet", href_ "css/normalize.css" ]
      link_ [ rel_ "stylesheet", href_ "css/skeleton.css" ]
    body_ $ do
      div_ [ class_ "container" ] $ do
        contents
  where
    renderedTitle =
      maybe "bitsbacker" ("bitsbacker | " <>) title

home = do
  template Nothing $ do
    h1_ "Hello, world!"

postSignup = do
  name  <- param "name"
  email <- param "email"
  text (name <> email)

signup = do
  template (Just "signup") $ do
    h1_ "Signup"
    form_ $ do
      label_ [ for_ "name" ] "Name"
      input_ [ id_ "name" ]
      label_ [ for_ "email" ] "Email"
      input_ [ id_ "email" ]


content = html . renderText

static path =
  staticPolicy (addBase path)

routes = do
  get  "/"       (content home)
  get  "/signup" (content signup)
  post "/signup" postSignup
  middleware (static "public")

main :: IO ()
main = do
  port <- getPort
  scotty port routes

