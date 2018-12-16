{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitsbacker.Data.User where

import Control.Concurrent (MVar, withMVar)
import Crypto.PasswordStore (makePassword)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
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

newtype UserId = UserId { getUserId :: Int64 }
    deriving (Show, Eq, Ord)

data User = User {
      userName           :: Username
    , userPassword       :: HashedPassword
    , userEmail          :: Email
    , userEmailConfirmed :: Bool
    , userPermissions    :: Permissions
    , userMaking         :: Text
    }

data UserPage = UserPage {
      userPageUser  :: User
    , userPageStats :: UserStats
    }

data UserStats = UserStats {
      userStatsBackers  :: Int
    , userStatsPerMonth :: Int
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

instance ToJSON User where
    toJSON User{..} =
        object [ "making"   .= userMaking
               , "name"     .= getUsername userName
               , "email"    .= getEmail userEmail
               ]

instance ToJSON UserPage where
    toJSON UserPage{..} =
        object [ "user"   .= userPageUser
               , "stats"  .= userPageStats
               ]

instance ToJSON UserStats where
    toJSON UserStats{..} =
        object [ "backers"  .= userStatsBackers
               , "perMonth" .= userStatsPerMonth
               ]

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
             , userMaking         = "bitsbacker"
           }

insertUser :: Connection -> User -> IO Int
insertUser conn user = do
  let fields = T.intercalate ", " userFields
      qs     = map (const "?") userFields
      qsc    = T.intercalate ", " qs
      q = "INSERT INTO users ("<>fields<>") values ("<>qsc<>")"
  execute conn (Query q) user
  fmap fromIntegral (lastInsertRowId conn)


getUser :: MVar Connection -> Username -> IO (Maybe (UserId, User))
getUser mvconn username = do
  let fields = T.intercalate ", " userFields
      q = "SELECT "<>fields<>" FROM users WHERE name = ? LIMIT 1"
  withMVar mvconn $ \conn -> withTransaction conn $ do
    muser <- query conn (Query q) (Only username)
    case listToMaybe muser of
      Nothing   -> return Nothing
      Just user_ ->
          do userId <- lastInsertRowId conn
             return (Just (UserId userId, user_))


getUserStats :: MVar Connection -> UserId -> IO UserStats
getUserStats _conn _userId = do
  return (UserStats 100 1000)
