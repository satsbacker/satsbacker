{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Test.Config
    ( testConfig
    ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Logger
import Network.Mail.SMTP (Address(..))

import Satsbacker.Config
import Satsbacker.Data.Site
import Satsbacker.DB
import Satsbacker.Templates
import Database.SQLite.Table
import Crypto.Macaroons

import Data.List.NonEmpty (NonEmpty(..))

import Bitcoin.Network

import qualified Data.Text as T



testPeerAddr :: PeerAddr
testPeerAddr =
  PeerAddr {
    peerAddrType = "ipv4"
  , peerAddr     = "127.0.0.1"
  , peerAddrPort = 1337
  }


testLightningConfig :: LightningConfig
testLightningConfig =
  LightningConfig {
    lncfgNetwork  = Regtest
  , lncfgPeerId   = T.replicate 64 "0"
  , lncfgPeerAddr = testPeerAddr :| []
  }


testConfig :: (MonadIO m, MonadLogger m) => m Config
testConfig = do
  port      <- liftIO getPort
  socketCfg <- getSocketConfig
  isProd    <- liftIO getIsProd
  let lncfg = testLightningConfig
  logInfoN $ "[ln] detected Bitcoin " <> T.pack (show (lncfgNetwork lncfg))
                 <> " from clightning"
  conn <- openDb (lncfgNetwork lncfg)
  liftIO (migrate conn)
  -- _payindex  <- liftIO (getPayIndex conn)
  msite     <- liftIO (fetchOne conn searchAny)
  site_     <- maybe (fail "could not find site config, corrupt?") return msite
  mvconn    <- liftIO (newMVar conn)
  mvnotify  <- liftIO newEmptyMVar
  templates <- liftIO loadTemplates
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
  logInfoN ("[site] using hostname " <> showSiteConfig site)
  return cfg

