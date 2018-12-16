
module Bitsbacker.Config where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Database.SQLite.Simple (Connection)
import Network.RPC.Config (SocketConfig(..))
import Control.Concurrent.MVar (MVar, newMVar)

import Bitsbacker.DB

data Config = Config {
      cfgConn :: MVar Connection
    , cfgRPC  :: MVar SocketConfig
    }

getConfig :: IO Config
getConfig = do
  socketCfg <- getSocketConfig
  conn <- openDb
  migrate conn
  mvconn <- newMVar conn
  mvsocket <- newMVar socketCfg
  return (Config mvconn mvsocket)


getSocketConfig :: IO SocketConfig
getSocketConfig = do
  path <- getRPCSocket
  return (SocketConfig path Nothing)


getRPCSocket :: IO FilePath
getRPCSocket = do
  mstrsocket <- lookupEnv "RPCSOCK"
  return (fromMaybe "/home/jb55/.lightning-bitcoin-rpc" mstrsocket)
