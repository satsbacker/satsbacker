{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-} 

module Network.RPC.CLightning.Channel
    ( Channel(..)
    , ListChannels(..)
    , ListChannelsResp(..)
    ) where

import Data.Aeson
import Data.Text (Text)
import Network.RPC.Common (Resp)
import Network.RPC.JsonRPC (noArgs)

data Channel = Channel {
    channelSource              :: Text
  , channelDestination         :: Text
  , channelShortId             :: Text
  , channelFlags               :: Int
  , channelActive              :: Bool
  , channelLastUpdate          :: Maybe Int
  , channelBaseFeeMillisatoshi :: Maybe Int
  , channelFeePerMillionth     :: Maybe Int
  , channelDelay               :: Maybe Int
  } deriving Show

instance FromJSON Channel where
  parseJSON (Object obj) =
    Channel <$> obj .: "source"
            <*> obj .: "destination"
            <*> obj .: "short_id"
            <*> obj .: "flags"
            <*> obj .: "active"
            <*> obj .:? "last_update"
            <*> obj .:? "base_fee_millisatoshi"
            <*> obj .:? "fee_per_millionth"
            <*> obj .:? "delay"
  parseJSON _ = fail "Channel is not an object"

instance FromJSON ListChannelsResp where
  parseJSON (Object obj) =
    ListChannelsResp <$> obj .: "channels"
  parseJSON _ = fail "ListChannelsResp is not an object"

newtype ListChannelsResp = ListChannelsResp { getChannelsResp :: [Channel] }
  deriving Show

data ListChannels = ListChannels
  deriving Show

type instance Resp ListChannels = ListChannelsResp

instance ToJSON ListChannels where
  toJSON _ = noArgs "getchannels"
