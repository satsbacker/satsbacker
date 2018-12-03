{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import System.Environment
import Lucid
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

getPort :: IO Int
getPort = do
  mstrport <- lookupEnv "PORT"
  return (fromMaybe 8002 (mstrport >>= readMaybe))

template title = do
  doctype_
  html_ $ do
    head_ $ do
      title_ renderedTitle
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]
      link_ [ rel_ "stylesheet", href_ "css/normalize.css" ]
      link_ [ rel_ "stylesheet", href_ "css/skeleton.css" ]
  where
    renderedTitle =
      maybe "bitsbacker" ("bitsbacker | " <>) title

home = do
  template Nothing
  body_ (h1_ "Hello, world!")

postSignup = do
  name  <- param "name"
  email <- param "email"
  text (name <> email)

signup = do
  template (Just "signup")
  body_ $ do
    h1_ "Signup"
    input_ [ id_ "name" ]
    input_ [ id_ "email" ]


content = html . renderText

routes = do
  get  "/"       (content home)
  get  "/signup" (content signup)
  post "/signup" postSignup

main :: IO ()
main = do
  port <- getPort
  scotty port routes

