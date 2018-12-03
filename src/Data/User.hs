{-# LANGUAGE RecordWildCards #-}

module Bitsbacker.Data.User where


import Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow as SQLite

import Database.PostgreSQL.Simple.ToRow as PG
import Database.PostgreSQL.Simple.FromRow as PG
import Database.PostgreSQL.Simple as PG

import Data.Text (Text)
import Data.ByteString (ByteString)

data User = User {
      userName  :: Text
    , userPass  :: ByteString
    , userEmail :: Text
    , userEmailConfirmed :: Bool
    }

instance SQLite.FromRow User where
  fromRow = User <$> SQLite.field
                 <*> SQLite.field
                 <*> SQLite.field
                 <*> SQLite.field

instance SQLite.ToRow User where
  toRow User{..} =
      SQLite.toRow ( userName
                   , userPass
                   , userEmail
                   , userEmailConfirmed
                   )

instance PG.FromRow User where
  fromRow = User <$> PG.field
                 <*> PG.field
                 <*> PG.field
                 <*> PG.field

instance PG.ToRow User where
  toRow User{..} =
      PG.toRow (userName
               ,userPass
               ,userEmail
               ,userEmailConfirmed
               )
