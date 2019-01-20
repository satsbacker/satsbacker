{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Test.Config
    ( testConfig
    ) where

import Control.Concurrent.MVar

import Satsbacker.Config
import Satsbacker.Data.Site
import Satsbacker.DB

import Database.SQLite.Simple

import Bitcoin.Network

import qualified Data.Text as T

testPeerAddr =
  PeerAddr {
    peerAddrType = "ipv4"
  , peerAddr     = "127.0.0.1"
  , peerAddrPort = "1337"
  }

testLightningConfig :: LightningConfig
testLightningConfig =
  LightningConfig {
    lncfgNetwork  = Regtest
  , lncfgPeerId   = T.replicate 64 '0'
  , lncfgPeerAddr = testPeerAddr
  }

logError :: String -> IO ()
logError = const (return ())

getConfig :: Connection -> IO Config
getConfig = do
  port <- getPort
  socketCfg <- getSocketConfig
  isProd <- getIsProd
  let lncfg = testLightningConfig
  logError $ "[ln] detected Bitcoin " ++ show (lncfgNetwork lncfg)
                 ++ " from clightning"
  conn <- openDb (lncfgNetwork lncfg)
  migrate conn
  payindex  <- getPayIndex conn
  msite     <- fetchOne conn searchAny
  site_     <- maybe (fail "could not find site config, corrupt?") return msite
  mvconn    <- newMVar conn
  mvnotify  <- newEmptyMVar
  templates <- loadTemplates
  let debugHostname = HostName ("localhost:" <> T.pack (show port))
      resolvedHost = if isProd
                      then siteHostName site_
                      else debugHostname
      site' = site_ { siteHostName = resolvedHost }
      site = if isProd
               then site'
               else site' { siteProtocol = Protocol "http" }
      cfg = Config {
              cfgConn         = mvconn
            , cfgRPC          = socketCfg
            , cfgPayNotify    = mvnotify
            , cfgLnConfig     = lncfg
            , cfgSite         = site
            , cfgSecret       = Secret "secret" -- TODO: macaroon secret
            , cfgEmail        = Address (Just "satsbacker") "noreply@satsbacker.com" -- TODO: macaroon secret
            , cfgTemplates    = templates
            }
  logError ("[site] using hostname " ++ T.unpack (showSiteConfig site))
  return cfg
