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

listPeers :: MonadIO m => SocketConfig -> m [Peer]
listPeers cfg = getPeersResp <$> rpc_ cfg "listpeers"

newAddr :: MonadIO m => SocketConfig -> Text -> m Text
newAddr cfg addrtype = do
  res :: Value <- rpc cfg "newaddr" [addrtype]
  maybe (fail "Could not decode address from newaddr") return
        (res ^? key "address" . _String)

listFunds :: MonadIO m => SocketConfig -> m [Output]
listFunds cfg = listFundsOutputs <$> rpc_ cfg "listfunds"

