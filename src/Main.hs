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

preamble title = do
  doctype_
  html_ $ do
    head_ $ do
      title_ title

routes = do
  get "/" . html . renderText $ do
    preamble "Title"
    body_ (h1_ "Hello, world!")

main :: IO ()
main = do
  port <- getPort
  scotty port routes

