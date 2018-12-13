{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.RPC.CLightning.Invoice
    ( Invoice(..)
    ) where

import Data.Text (Text)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Data.ByteString (ByteString)

import qualified Data.Text as T

import Network.Lightning.Bolt11 (Bolt11)

instance FromJSON Invoice where
    parseJSON (Object obj) = Invoice <$> obj .: "bolt11"

newtype Invoice = Invoice { getRawInvoice :: Text }
    deriving Show
