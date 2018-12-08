{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.RPC.Internal
    ( sockRequest
    ) where

import Data.ByteString
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromMaybe)
import Network.RPC.Common (defaultTimeout)
import Network.RPC.Config (SocketConfig(..))
import Network.RPC.Error
import Network.Socket.ByteString
import Network.Socket (socket, Family(AF_UNIX), SocketType(Stream), connect,
                       SockAddr(SockAddrUnix), close)

import qualified Data.ByteString as BS
import qualified Data.DList as DL
import qualified Data.ByteString.Lazy as Lazy

sockRequest :: MonadIO m => SocketConfig -> ByteString -> m (Either RPCError Lazy.ByteString)
sockRequest SocketConfig{..} bs = liftIO $ timeout tout $ do
  soc <- socket AF_UNIX Stream 0
  catching connectionError (connect soc (SockAddrUnix rpcPath))
  catching writeError (sendAll soc bs)
  out <- catching readError (readAll soc)
  close soc
  return out
  where
    tout        = fromMaybe defaultTimeout rpcTimeout
    readAll soc = fmap (Lazy.fromChunks . DL.toList) (readChunks soc DL.empty)
    readChunks soc chunks =
      do chunk <- recv soc 128
         let cs = DL.snoc chunks chunk
         if BS.length chunk < 128
           then return cs
           else readChunks soc cs
