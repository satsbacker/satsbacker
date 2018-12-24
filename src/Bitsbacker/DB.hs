{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Bitsbacker.DB
    ( migrate
    , openDb
    ) where

import Control.Monad (unless)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Foldable (traverse_)
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing)

import qualified Data.ByteString as BS

import Bitcoin.Network
import Bitsbacker.Logging

-- ensureDb :: FilePath -> IO ()
-- ensureDb dataPath = do

migrations :: [Query]
migrations = [
    "CREATE TABLE version (version INTEGER)",      -- 0
    "INSERT INTO version (version) VALUES (1)",    -- 1

    "CREATE TABLE users (id INTEGER PRIMARY KEY,\
     \ password TEXT, \
     \ email TEXT, \
     \ email_confirmed INTEGER, \
     \ name TEXT unique, \
     \ making TEXT, \
     \ created_at INTEGER NOT_NULL DEFAULT CURRENT_TIMESTAMP, \
     \ permissions INTEGER) ",    -- 2

     "CREATE TABLE invoices (invoiceId TEXT PRIMARY KEY,\
     \ tier_id INTEGER,\
     \ email TEXT)", -- 3

     "CREATE TABLE tiers (id INTEGER PRIMARY KEY,\
     \ user_id INTEGER NOT NULL,\
     \ description TEXT,\
     \ quota INTEGER,\
     \ type INTEGER not null,\
     \ amount_fiat INTEGER,\
     \ amount_msats INTEGER,\
     \ created_at INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP,\
     \ state INTEGER)",        -- 4

     "CREATE TABLE subscriptions (id INTEGER PRIMARY KEY,\
     \ for_user INTEGER NOT NULL,\
     \ user_id INTEGER NOT NULL,\
     \ valid_until INTEGER NOT NULL,\
     \ tier_id INTEGER NOT NULL,\
     \ created_at INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP\
     \)", -- 5

     "CREATE TABLE payindex (payindex INTEGER)", -- 6
     "INSERT INTO payindex (payindex) VALUES (0)" -- 7
  ]

hasVersionTable :: Connection -> IO Bool
hasVersionTable conn = do
  res <- query_ conn "SELECT name from sqlite_master WHERE type='table' and name='version'"
           :: IO [Only Text]
  return (not (null res))

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

openDb :: BitcoinNetwork -> IO Connection
openDb network =
  let dbfile = case network of
                 Mainnet -> "bitsbacker.db"
                 Testnet -> "bitsbacker-testnet.db"
  in
    do logError ("[db] using " ++ dbfile)
       open dbfile


migrate :: Connection -> IO ()
migrate conn = do
  version <- getDbVersion conn
  let stmts          = drop version migrations
      latestVersion  = length migrations
  unless (null stmts) $ do
    if latestVersion < version then
      fail ("Refusing to migrate down from version "
              ++ show version ++ " to " ++ show latestVersion)
    else do
      withTransaction conn $ do
        traverse_ (execute_ conn) stmts
        updateVersion conn latestVersion
      saveMigration version latestVersion stmts
