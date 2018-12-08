{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.RPC
    ( rpc, rpc_
    , module X
    ) where

import Data.Aeson
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (toStrict)
import Data.Bifunctor (bimap)
import Data.Text (Text)

import Network.RPC.CLightning.Commands as X
import Network.RPC.Common (Resp)
import Network.RPC.Config (SocketConfig(..))
import Network.RPC.Config as X
import Network.RPC.Error
import Network.RPC.Internal (sockRequest)
import Network.RPC.JsonRPC (makeRequest)

import qualified Data.ByteString.Char8 as B8

type instance Resp Value = Value

rpc :: (MonadIO m, ToJSON params, FromJSON resp)
    => SocketConfig -> Text -> params -> m resp
rpc cfg method params = do
  let req = encode (makeRequest method params)
  mres <- sockRequest cfg (toStrict req)
  case mres of
    Right res ->
      either (fail . show . jsonDecodeError . B8.pack)
             (return . getCRPCResp)
             (eitherDecode res)
    Left e -> fail (show e)


rpc_ :: (MonadIO m, FromJSON resp)
     => SocketConfig -> Text -> m resp
rpc_ cfg method = rpc cfg method (mempty :: Array)
