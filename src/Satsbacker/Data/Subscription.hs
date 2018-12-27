{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Data.Subscription
    ( Subscription(..)
    ) where

import Data.Text (Text)

import Satsbacker.Data.Tiers (TierId(..))
import Satsbacker.Data.User (UserId(..))
import Satsbacker.Data.Email (Email)
import Satsbacker.DB.Table (Table(..))

import Database.SQLite.Simple

instance Table Subscription where
    tableName _ = "subscriptions"
    tableFields _ = subscriptionFields

subscriptionFields :: [Text]
subscriptionFields =
    [ "for_user"
    , "user_id" 
    , "user_email" 
    , "user_cookie" 
    , "valid_until"
    , "tier_id"
    ]

data Subscription = Subscription
  { subForUser     :: UserId
  , subPayerId     :: Maybe UserId
  , subPayerEmail  :: Email
  , subPayerCookie :: Maybe Text
  , subValidUntil  :: Int
  , subTierId      :: TierId
  }


instance ToRow Subscription where
    toRow Subscription{..} =
        toRow ( getUserId subForUser
              , fmap getUserId subPayerId
              , subPayerEmail
              , subPayerCookie
              , subValidUntil
              , subTierId
              )
