{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import System.Environment
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Lucid
import Html

getPort :: IO Int
getPort = do
  mstrport <- lookupEnv "PORT"
  return (fromMaybe 8002 (mstrport >>= readMaybe))

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
      textInput "name"
      textInput "email"

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

