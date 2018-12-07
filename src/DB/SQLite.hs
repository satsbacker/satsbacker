{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Bitsbacker.DB.SQLite
    ( migrate
    ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing)

import qualified Data.ByteString as BS
import qualified Data.Text as T

newtype DBVersion = DBVersion { getDBVersion :: Int }


-- ensureDb :: FilePath -> IO ()
-- ensureDb dataPath = do

migrations :: [Query]
migrations = [
    "CREATE TABLE version (version INTEGER)",
    "INSERT INTO version (version) VALUES (1)"
  ]

hasVersionTable :: Connection -> IO Bool
hasVersionTable conn = do
  res <- query_ conn "SELECT name from sqlite_master WHERE type='table' and name='version'"
           :: IO [Only Text]
  return (length res == 1)


getDbVersion :: Connection -> IO Int
getDbVersion conn = do
  hasVersion <- hasVersionTable conn
  if not hasVersion
    then return 0
    else do
      res <- fmap listToMaybe (query_ conn "SELECT version FROM version LIMIT 1")
      return (maybe 0 fromOnly res)

updateVersion :: Connection -> Int -> IO ()
updateVersion conn ver =
  execute conn "UPDATE version SET version = ?" (Only ver)

-- TODO: dev-only
saveMigration :: Int -> Int -> [Query] -> IO ()
saveMigration from to stmts = do
  createDirectoryIfMissing True ".migrations"
  let fileName = show from ++ "-to-" ++ show to ++ ".txt"
      contents = foldMap ((<>"\n") . fromQuery) stmts
  BS.writeFile (".migrations/" ++ fileName) (encodeUtf8 contents)

migrate :: IO ()
migrate = do
  conn <- open "bitsbacker.db"
  version <- getDbVersion conn
  let stmts          = drop version migrations
      latestVersion  = length migrations
  unless (null stmts) $ do
    if latestVersion < version then
      fail ("Refusing to migrate down from version "
              ++ show version ++ " to " ++ show latestVersion)
    else do
      withTransaction conn $ do
        traverse (execute_ conn) stmts
        updateVersion conn latestVersion
      saveMigration version latestVersion stmts

