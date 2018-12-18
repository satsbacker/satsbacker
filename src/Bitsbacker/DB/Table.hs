{-# LANGUAGE OverloadedStrings #-}

module Bitsbacker.DB.Table
    ( Table(..)
    , insert
    , insertL
    ) where

import Data.Text (Text)
import Control.Concurrent.MVar (MVar, withMVar)
import Database.SQLite.Simple

import qualified Data.Text as T

class Table a where
    tableName   :: a -> Text
    tableFields :: a -> [Text]

insert :: (Table a, ToRow a) => Connection -> a -> IO Int
insert conn row = do
  let tfields = tableFields row
      fields = T.intercalate ", " tfields
      qs     = map (const "?") tfields
      qsc    = T.intercalate ", " qs
      q = "INSERT INTO "<>tableName row<>" ("<>fields<>") values ("<>qsc<>")"
  execute conn (Query q) row
  fmap fromIntegral (lastInsertRowId conn)


insertL :: (Table a, ToRow a) => MVar Connection -> a -> IO Int
insertL mvconn row = withMVar mvconn $ \conn -> insert conn row
