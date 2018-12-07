{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitsbacker.Data.User where

import Data.Maybe (listToMaybe)
import Crypto.PasswordStore (makePassword)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

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
    , userMaking         :: Text
    }

userFields :: [Text]
userFields = [
    "name"
  , "password"
  , "email"
  , "email_confirmed"
  , "permissions"
  , "making"
  ]

instance FromRow User where
  fromRow = User <$> field
                 <*> field
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
            , userMaking
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
             , userMaking         = "their backer page"
           }

insertUser :: Connection -> User -> IO Int
insertUser conn user = do 
  let fields = T.intercalate ", " userFields
      qs     = map (const "?") userFields
      qsc    = T.intercalate ", " qs
      q = "INSERT INTO users ("<>fields<>") values ("<>qsc<>")"
  execute conn (Query q) user
  fmap fromIntegral (lastInsertRowId conn)

getUser :: Connection -> Username -> IO (Maybe User)
getUser conn user = do
  let fields = T.intercalate ", " userFields
      q = "SELECT "<>fields<>" FROM users WHERE name = ? LIMIT 1"
  users <- query conn (Query q) (Only user)
  return (listToMaybe users)
