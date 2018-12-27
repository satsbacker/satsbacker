{-# LANGUAGE ScopedTypeVariables #-}

module Network.RPC.CLightning where

import Control.Lens
import Data.Maybe (maybe)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson.Lens (key, _String)
import Data.Aeson (Value)
import Data.Text (Text)
import Network.RPC
import Network.RPC.CLightning.Peer
import Network.RPC.CLightning.Output
import Satsbacker.Data.Invoice

listpeers :: MonadIO m => SocketConfig -> m [Peer]
listpeers cfg = getPeersResp <$> rpc_ cfg "listpeers"

newaddr :: MonadIO m => SocketConfig -> Text -> m Text
newaddr cfg addrtype = do
  res :: Value <- rpc cfg "newaddr" [addrtype]
  maybe (fail "Could not decode address from newaddr") return
        (res ^? key "address" . _String)

listfunds :: MonadIO m => SocketConfig -> m [Output]
listfunds cfg = listFundsOutputs <$> rpc_ cfg "listfunds"

listinvoices :: MonadIO m => SocketConfig -> Text -> m [Invoice]
listinvoices cfg invId =
  getCLInvoices <$> rpc cfg "listinvoices" [invId]

waitinvoice :: MonadIO m => SocketConfig -> Text -> m (Maybe Invoice)
waitinvoice cfg invId =
  fmap getCLInvoice <$> rpc cfg "waitinvoice" [invId]
