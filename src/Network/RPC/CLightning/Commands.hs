{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

module Network.RPC.CLightning.Commands
    ( CRPCResp(..)
    , ListPeersResp(..)
    , ListChannels(..)
    , ListChannelsResp(..)
    ) where

import Network.RPC.CLightning.Peer
import Network.RPC.CLightning.Channel
import Data.Aeson

newtype CRPCResp a = CRPCResp { getCRPCResp :: a }
  deriving Show

instance FromJSON a => FromJSON (CRPCResp a) where
  parseJSON (Object obj) =
    CRPCResp <$> obj .: "result"
  parseJSON _ = fail "CRPCRsp is not an object"

