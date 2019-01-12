{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Data.User where

import Crypto.PasswordStore (makePassword)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Bitcoin.Denomination

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

import Database.SQLite.Table (Table(..))
import Satsbacker.Data.Email
import Satsbacker.AmountConfig

import qualified Data.Text as T

newtype Username = Username { getUsername :: Text }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype Plaintext = Plaintext { getPlaintext :: Text }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype HashedPassword = HashedPassword { getHashedPassword :: ByteString }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype Permissions = Permissions { getPermissions :: Int }
    deriving (Show, Eq, Ord, ToField, FromField)

newtype UserId = UserId { getUserId :: Int }
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
    , userStatsPerMonth :: MSats
    }

instance FromRow UserStats where
    fromRow =
        UserStats <$> field
                  <*> fmap MSats field

userFields :: [Text]
userFields = [
    "name"
  , "password"
  , "email"
  , "email_confirmed"
  , "permissions"
  , "making"
  ]


instance Table User where
    tableName _   = "users"
    tableFields _ = userFields


instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow user =
      let
          User f1 f2 f3 f4 f5 f6 = user
      in
        toRow (f1, f2, f3, f4, f5, f6)


instance ToJSON User where
    toJSON User{..} =
        object [ "making"   .= userMaking
               , "name"     .= getUsername userName
               , "email"    .= getEmail userEmail
               ]

userPageToJSON :: AmountConfig -> UserPage -> Value
userPageToJSON acfg UserPage{..} =
  object [ "user"   .= userPageUser
          , "stats" .= userStatsToJSON acfg userPageStats
          ]

userStatsToJSON :: AmountConfig -> UserStats -> Value
userStatsToJSON acfg UserStats{..} =
  object [ "backers"  .= userStatsBackers
          , "perMonth" .= renderAmount acfg userStatsPerMonth
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
             , userMaking         = "cool stuff"
           }

commaUserFields :: Text
commaUserFields =
    T.intercalate ", " userFields

getUserById :: Connection -> UserId -> IO (Maybe User)
getUserById conn (UserId userId) = do
  let q = "SELECT "<>commaUserFields<>" FROM USERS WHERE id = ? LIMIT 1"
  users <- query conn (Query q) (Only userId)
  return (listToMaybe users)

getUser :: Connection -> Username -> IO (Maybe (UserId, User))
getUser conn username = do
  let q = "SELECT id,"<>commaUserFields<>" FROM users WHERE name = ? LIMIT 1"
  muser <- query conn (Query q) (Only username)
  case listToMaybe muser of
    Nothing   -> return Nothing
    Just (Only userId :. user) ->
      return (Just (UserId userId, user))


getUserStats :: Connection -> UserId -> IO (Maybe UserStats)
getUserStats conn (UserId userId) =
  let q = "SELECT count(s.tier_id) as backers, coalesce(sum(t.amount_msats),0) as per_month \
          \FROM subscriptions s \
          \INNER JOIN tiers t ON t.id = s.tier_id \
          \WHERE s.for_user = ? "
  in
    fmap listToMaybe $ query conn (Query q) (Only userId)
