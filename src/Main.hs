{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import System.Environment (getArgs)

import Bitsbacker.Cli
import Bitsbacker.DB (migrate, openDb)

import qualified Data.Text as T


main :: IO ()
main = do
  args <- fmap (map T.pack) getArgs
  mainWith args


mainWith :: [Text] -> IO ()
mainWith args = do
  conn <- openDb
  migrate conn

  case args of
    (x:xs) -> processArgs conn x xs
    []     -> usage
