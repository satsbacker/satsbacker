{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bitsbacker.Config where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple (Connection, execute, query_, Only(..))
import Network.RPC.CLightning.Invoice
import Network.RPC (rpc)
import System.Environment (lookupEnv)

import Bitsbacker.DB
import Bitsbacker.Logging

import Network.RPC.Config (SocketConfig(..))

data Config = Config {
      cfgConn      :: MVar Connection
    , cfgRPC       :: SocketConfig
    , cfgPayNotify :: MVar WaitInvoice
    }

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

getConfig :: IO Config
getConfig = do
  socketCfg <- getSocketConfig
  conn <- openDb
  migrate conn
  payindex <- getPayIndex conn
  mvconn <- newMVar conn
  mvnotify <- newEmptyMVar
  let cfg = Config mvconn socketCfg mvnotify
  _ <- forkIO (waitInvoices 0 payindex cfg)
  return cfg


getSocketConfig :: IO SocketConfig
getSocketConfig = do
  path <- getRPCSocket
  return (SocketConfig path Nothing)


getRPCSocket :: IO FilePath
getRPCSocket = do
  mstrsocket <- lookupEnv "RPCSOCK"
  return (fromMaybe "/home/jb55/.lightning-bitcoin-rpc" mstrsocket)
