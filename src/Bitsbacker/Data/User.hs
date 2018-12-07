{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitsbacker.Data.User where

import Database.SQLite.Simple
import Crypto.PasswordStore (makePassword)

import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

-- import Database.PostgreSQL.Simple.ToRow as PG
-- import Database.PostgreSQL.Simple.FromRow as PG
-- import Database.PostgreSQL.Simple as PG

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

newtype Email = Email { getEmail :: Text }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype Username = Username { getUsername :: Text }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype Plaintext = Plaintext { getPlaintext :: Text }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype HashedPassword = HashedPassword { getHashedPassword :: ByteString }
    deriving (Show, Eq, Ord, ToField, FromField)

data User = User {
      userName  :: Username
    , userPass  :: HashedPassword
    , userEmail :: Email
    , userEmailConfirmed :: Bool
    }

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow User{..} =
      toRow ( userName
            , userPass
            , userEmail
            , userEmailConfirmed
            )

-- TODO: PG
-- instance PG.FromRow User where
--   fromRow = User <$> PG.field
--                  <*> PG.field
--                  <*> PG.field
--                  <*> PG.field

-- instance PG.ToRow User where
--   toRow User{..} =
--       PG.toRow (userName
--                ,userPass
--                ,userEmail
--                ,userEmailConfirmed
--                )

createUser :: Username -> Email -> Plaintext -> IO User
createUser name email (Plaintext password) = do
  hashedPass <- makePassword (encodeUtf8 password) 17
  return $ User {
               userPass  = HashedPassword hashedPass
             , userName  = name
             , userEmail = email
             , userEmailConfirmed = False
           }

insertUser :: Connection -> User -> IO Int
insertUser conn user = do 
    execute conn "INSERT INTO users ?" user
    fmap fromIntegral (lastInsertRowId conn)
