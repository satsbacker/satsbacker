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

import Database.SQLite.Table (Table(..))

newtype Protocol = Protocol { getProtocol :: Text }

newtype HostName = HostName { getHost :: Text }
    deriving (ToJSON, ToField, FromField)

data Site = Site {
      siteName     :: Text
    , siteHostName :: HostName
    , siteProtocol :: Protocol
    }

siteFields :: [Text]
siteFields = ["name", "hostname"]

instance ToJSON Site where
    toJSON site =
        let
            Site name hostname _ = site
        in
          object [ "name"     .= name
                 , "hostname" .= hostname
                 ]


instance ToRow Site where
    toRow site =
        let
            Site f1 f2 _f = site
        in
            toRow (f1, f2)


instance FromRow Site where
    fromRow =
        Site <$> field
             <*> field
             <*> pure (Protocol "https")

instance Table Site where
    tableName _   = "site"
    tableFields _ = siteFields
