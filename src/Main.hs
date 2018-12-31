{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import System.Environment (getArgs)

import Satsbacker.Cli
import Satsbacker.Config

import qualified Data.Text as T


main :: IO ()
main = do
  args <- fmap (map T.pack) getArgs
  mainWith args


mainWith :: [Text] -> IO ()
mainWith args = do
  cfg <- getConfig
  case args of
    (x:xs) -> processArgs cfg x xs
    []     -> usage
