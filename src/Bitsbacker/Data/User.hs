{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Text as T

newtype Email = Email { getEmail :: Text }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype Username = Username { getUsername :: Text }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype Plaintext = Plaintext { getPlaintext :: Text }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype HashedPassword = HashedPassword { getHashedPassword :: ByteString }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype Permissions = Permissions { getPermissions :: Int }
    deriving (Show, Eq, Ord, ToField, FromField)

data User = User {
      userName           :: Username
    , userPassword       :: HashedPassword
    , userEmail          :: Email
    , userEmailConfirmed :: Bool
    , userPermissions    :: Permissions
    }

userFields :: [Text]
userFields = [
    "name"
  , "password"
  , "email"
  , "email_confirmed"
  , "permissions"
  ]

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow User{..} =
      toRow ( userName
            , userPassword
            , userEmail
            , userEmailConfirmed
            , userPermissions
            )

defaultPermissions :: Permissions
defaultPermissions = Permissions 0

createUser :: Plaintext -> IO User
createUser (Plaintext password) = do
  hashedPass <- makePassword (encodeUtf8 password) 17
  return $ User {
               userPassword       = HashedPassword hashedPass
             , userName           = Username ""
             , userEmail          = Email ""
             , userEmailConfirmed = True
             , userPermissions    = defaultPermissions
           }

insertUser :: Connection -> User -> IO Int
insertUser conn user = do 
  let fields = T.intercalate ", " userFields
      qs     = map (const "?") userFields
      qsc    = T.intercalate ", " qs
      q = "INSERT INTO users ("<>fields<>") values ("<>qsc<>")"
  execute conn (Query q) user
  fmap fromIntegral (lastInsertRowId conn)
