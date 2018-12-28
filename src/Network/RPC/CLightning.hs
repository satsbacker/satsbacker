{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.RPC.CLightning where

import Data.Maybe (maybe)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value(..))
import Data.Text (Text)
import Network.RPC
import Network.RPC.CLightning.Peer
import Network.RPC.CLightning.Output
import Satsbacker.Data.Invoice

import qualified Data.HashMap.Lazy as Map

listpeers :: MonadIO m => SocketConfig -> m [Peer]
listpeers cfg = getPeersResp <$> rpc_ cfg "listpeers"

keyStr :: Text -> Value -> Maybe Text
keyStr str (Object obj) =
  case Map.lookup str obj of
    Just (String txt) -> Just txt
    Just _            -> Nothing
    Nothing           -> Nothing
keyStr _ _ = Nothing

newaddr :: MonadIO m => SocketConfig -> Text -> m Text
newaddr cfg addrtype = do
  res :: Value <- rpc cfg "newaddr" [addrtype]
  maybe (fail "Could not decode address from newaddr") return
        (keyStr "address" res)

listfunds :: MonadIO m => SocketConfig -> m [Output]
listfunds cfg = listFundsOutputs <$> rpc_ cfg "listfunds"

listinvoices :: MonadIO m => SocketConfig -> Text -> m [Invoice]
listinvoices cfg invId =
  getCLInvoices <$> rpc cfg "listinvoices" [invId]

waitinvoice :: MonadIO m => SocketConfig -> Text -> m (Maybe Invoice)
waitinvoice cfg invId =
  fmap getCLInvoice <$> rpc cfg "waitinvoice" [invId]
