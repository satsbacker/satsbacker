{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bitsbacker.Config where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Data.Functor (void)
import Data.Maybe (fromMaybe, isJust)
import Database.SQLite.Simple (Connection, execute, query_, Only(..))
import Network.RPC (rpc)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Data.Aeson

import Bitcoin.Network
import Bitsbacker.DB
import Bitsbacker.Logging

import Network.RPC.CLightning.Invoice
import Network.RPC

import Network.RPC.Config (SocketConfig(..))

import qualified Data.HashMap.Lazy as Map

data Config = Config {
      cfgConn      :: MVar Connection
    , cfgRPC       :: SocketConfig
    , cfgPayNotify :: MVar WaitInvoice
    , cfgNetwork   :: BitcoinNetwork
    }

instance ToJSON Config where
    toJSON Config{..} =
        object
           [ "network"    .= cfgNetwork
           , "is_testnet" .= (cfgNetwork == Testnet)
           ]

getPayIndex :: Connection -> IO Int
getPayIndex conn =
  fromOnly . head <$> query_ conn "SELECT payindex FROM payindex"

persistPayIndex :: Connection -> Int -> IO ()
persistPayIndex conn payind =
  execute conn "UPDATE payindex SET payindex = ?" (Only payind)

waitInvoices :: Int -> Int -> Config -> IO ()
waitInvoices 10 _ _ = fail "Too many errors when waiting for payments"
waitInvoices !errs !payindex !cfg@Config{..} = do
  ewi :: Either SomeException WaitInvoice <-
           try $ rpc cfgRPC "waitanyinvoice" [payindex]
  case ewi of
    Left err -> logError (show err) >> waitInvoices (errs + 1) payindex cfg
    Right !wi@(WaitInvoice (!index, !_inv)) -> do
      isEmpty <- isEmptyMVar cfgPayNotify
      if isEmpty
        then putMVar cfgPayNotify wi
        else void (swapMVar cfgPayNotify wi)
      print wi
      withMVar cfgConn $ \conn -> persistPayIndex conn index
      waitInvoices 0 index cfg

getNetwork :: SocketConfig -> IO BitcoinNetwork
getNetwork cfg = do
  mval    <- timeout (5 * 1000000) (rpc_ cfg "getinfo")
  val     <- maybe timeouterr return mval
  network <- maybe err return (key "network" val)
  maybe err return (parseNetwork network)
  where
    timeouterr = fail "timeout during clightning getinfo call"
    err = fail "getinfo: network key not found"
    key str (Object obj) =
      case Map.lookup str obj of
        Just (String txt) -> Just txt
        Just _            -> Nothing
        Nothing           -> Nothing
    key _ _ = Nothing

getConfig :: IO Config
getConfig = do
  socketCfg <- getSocketConfig
  network <- getNetwork socketCfg
  logError $ "[ln] detected Bitcoin " ++ show network ++ " from clightning"
  conn <- openDb network
  migrate conn
  payindex <- getPayIndex conn
  mvconn <- newMVar conn
  mvnotify <- newEmptyMVar
  let cfg = Config mvconn socketCfg mvnotify network
  _ <- forkIO (waitInvoices 0 payindex cfg)
  return cfg


getSocketConfig :: IO SocketConfig
getSocketConfig = do
  path <- getRPCSocket
  return (SocketConfig path Nothing)


getRPCSocket :: IO FilePath
getRPCSocket = do
  mstrsocket <- lookupEnv "RPCSOCK"
  let from = if isJust mstrsocket
               then "RPCSOCK env"
               else "default setting"
      cfg = fromMaybe "/home/jb55/.lightning-bitcoin-rpc" mstrsocket
  logError $ "[rpc] using " ++ cfg  ++ " from " ++ from
  return cfg
