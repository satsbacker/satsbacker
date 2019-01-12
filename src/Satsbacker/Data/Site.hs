{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Data.Site
    ( Site(..)
    , HostName(..)
    , Protocol(..)
    ) where

import Data.Text (Text)
import Data.Aeson
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Satsbacker.AmountConfig

import Database.SQLite.Table (Table(..))

newtype Protocol = Protocol { getProtocol :: Text }

newtype HostName = HostName { getHost :: Text }
    deriving (ToJSON, ToField, FromField)

data Site = Site {
      siteName      :: Text
    , siteHostName  :: HostName
    , siteProtocol  :: Protocol
    , siteAmountCfg :: AmountConfig
    }

siteFields :: [Text]
siteFields = ["name", "hostname", "amount_type"]

instance ToJSON Site where
    toJSON site =
        let
            Site name hostname _ acfg = site
        in
          object [ "name"        .= name
                 , "hostname"    .= hostname
                 , "amount_type" .= renderDenomination acfg
                 ]


instance ToRow Site where
    toRow site =
        let
            Site f1 f2 _ f3 = site
        in
            toRow (f1, f2, f3)


instance FromRow Site where
    fromRow =
        Site <$> field
             <*> field
             <*> pure (Protocol "https")
             <*> field

instance Table Site where
    tableName _   = "site"
    tableFields _ = siteFields
