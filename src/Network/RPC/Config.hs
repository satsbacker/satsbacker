
module Network.RPC.Config
    ( SocketConfig(..)
    ) where

data SocketConfig =
  SocketConfig {
    rpcPath    :: FilePath
  , rpcTimeout :: Maybe Int
  }
  deriving (Show)
