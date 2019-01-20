{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Data.Subscription
    ( Subscription(..)
    , SubId(..)
    ) where

import Data.Text (Text)

import Satsbacker.Data.Tiers (TierId(..))
import Satsbacker.Data.User (UserId(..))
import Satsbacker.Data.InvoiceId (InvId(..))
import Satsbacker.Data.Email (Email)
import Database.SQLite.Table (Table(..))

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
    , "invoice_id"
    ]


newtype SubId = SubId { getSubId :: Int }


data Subscription = Subscription
  { subForUser     :: UserId
  , subPayerId     :: Maybe UserId
  , subPayerEmail  :: Email
  , subPayerCookie :: Maybe Text
  , subValidUntil  :: Int
  , subTierId      :: TierId
  , subInvoiceId   :: InvId
  }


instance ToRow Subscription where
    toRow sub =
        let
            Subscription f1 f2 f3 f4 f5 f6 f7 = sub
        in
          toRow ( getUserId f1
                , fmap getUserId f2
                , f3
                , f4
                , f5
                , f6
                , getInvId f7
                )
