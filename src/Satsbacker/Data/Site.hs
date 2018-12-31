{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Data.Site
    ( Site(..)
    ) where

import Data.Text (Text)
import Data.Aeson
import Database.SQLite.Simple

import Satsbacker.DB.Table (Table(..))


data Site = Site {
      siteCfgName :: Text
    }

siteFields :: [Text]
siteFields = ["name"]

instance ToJSON Site where
    toJSON site =
        let
            Site name = site
        in
          object [ "name" .= name ]


instance ToRow Site where
    toRow site =
        let
            Site f1 = site
        in
            toRow (Only f1)


instance FromRow Site where
    fromRow =
        Site <$> field

instance Table Site where
    tableName _   = "site"
    tableFields _ = siteFields
