{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Config where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, execute, query_, Only(..))
import Foreign.C.Types (CTime(..))
import Network.Mail.SMTP (Address(..))
import Network.RPC (rpc)
import System.Environment (lookupEnv)
import System.Posix.Time (epochTime)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Bitcoin.Network
import Crypto.Macaroons (Secret(..))
import Database.SQLite.Table
import Network.RPC
import Network.RPC.Config (SocketConfig(..))
import Satsbacker.DB
import Satsbacker.Data.Invoice
import Satsbacker.Data.InvoiceId
import Satsbacker.Data.Site (Site(..), HostName(..), Protocol(..))
import Satsbacker.Data.Subscription
import Satsbacker.Data.Tiers (TierDef(..))
import Satsbacker.Logging
import Satsbacker.Templates
import Text.Mustache (Template)

import qualified Data.HashMap.Lazy as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data Config = Config {
      cfgConn      :: MVar Connection
    , cfgRPC       :: SocketConfig
    , cfgPayNotify :: MVar WaitInvoice
    , cfgLnConfig  :: LightningConfig
    , cfgSite      :: Site
    , cfgSecret    :: Secret
    , cfgEmail     :: Address
    , cfgTemplates :: Template
    }

cfgNetwork :: Config -> BitcoinNetwork
cfgNetwork = lncfgNetwork . cfgLnConfig

data PeerAddr = PeerAddr {
      peerAddrType :: Text
    , peerAddr     :: Text
    , peerAddrPort :: Int
    }

instance FromJSON PeerAddr where
    parseJSON (Object obj) =
        PeerAddr <$> obj .: "type"
                 <*> obj .: "address"
                 <*> obj .: "port"
    parseJSON _ = fail "expected object for PeerAddr"


data LightningConfig = LightningConfig {
      lncfgNetwork  :: BitcoinNetwork
    , lncfgPeerId   :: Text
    , lncfgPeerAddr :: NonEmpty PeerAddr
    }

instance FromJSON LightningConfig where
    parseJSON v@(Object obj) =
        LightningConfig <$> parseNetworkValue v
                        <*> obj .: "id"
                        <*> obj .: "address"
    parseJSON _ = fail "could not parse clightning getinfo config"



instance ToJSON Config where
    toJSON cfg =
        let
            Config _ _ _ lncfg site _ (Address _ email) _ = cfg
            network = lncfgNetwork lncfg
        in
          object
            [ "network"     .= network
            , "is_testnet"  .= (network == Testnet)
            , "peer"        .= showLnPeer lncfg
            , "site"        .= site
            , "email"       .= email
            ]


showLnPeer :: LightningConfig -> Text
showLnPeer LightningConfig{..} =
    showPeer (NE.head lncfgPeerAddr) lncfgPeerId

showPeer :: PeerAddr -> Text -> Text
showPeer PeerAddr{..} peerId =
    if peerAddrPort == 9735
       then peerId <> "@" <> peerAddr
       else peerId <> "@" <> peerAddr <> ":" <> T.pack (show peerAddrPort)


getPayIndex :: Connection -> IO Int
getPayIndex conn =
  fromOnly . head <$> query_ conn "SELECT payindex FROM payindex"


persistPayIndex :: Connection -> Int -> IO ()
persistPayIndex conn payind =
  execute conn "UPDATE payindex SET payindex = ?" (Only payind)


persistSubFromInvId :: MVar Connection -> InvId -> IO (Maybe Subscription)
persistSubFromInvId mvconn (InvId invId) = do
  minvRef <- withMVar mvconn $ \conn ->
    fetchOne conn (search "invoice_id" invId)

  case minvRef of
    Nothing -> do logError $ "[WARN] couldn't find paid invoiceId " ++ show invId
                  return Nothing
    Just InvoiceRef{..} -> do
      mtier <- withMVar mvconn $ \conn ->
        fetchOne conn (search "id" invRefTierId)

      TierDef{..} <- maybe (fail "could not find tier for invoice") return mtier

      CTime timestamp <- epochTime

      let sub = Subscription {
                  subForUser     = tierUserId
                , subPayerId     = invRefPayerId
                , subPayerEmail  = invRefEmail
                , subPayerCookie = Nothing -- TODO: do we need cookies?
                , subValidUntil  = fromIntegral timestamp + 2678400
                    -- NOTE: always 31 days
                , subTierId      = invRefTierId
                , subInvoiceId   = invRefInvoiceId
                }

      _ <- withMVar mvconn $ \conn -> insert conn sub
      return (Just sub)


waitInvoices :: Int -> Int -> Config -> IO ()
waitInvoices 10 _ _ = fail "Too many errors when waiting for payments"
waitInvoices !errs !payindex !cfg@Config{..} = do
  ewi :: Either SomeException WaitInvoice <-
           try $ rpc cfgRPC "waitanyinvoice" [payindex]
  case ewi of
    Left err -> logError (show err) >> waitInvoices (errs + 1) payindex cfg
    Right !wi@(WaitInvoice (!index, !inv)) -> do
      _sub <- persistSubFromInvId cfgConn (invoiceId (getCLInvoice inv))
      isEmpty <- isEmptyMVar cfgPayNotify
      if isEmpty
        then putMVar cfgPayNotify wi
        else void (swapMVar cfgPayNotify wi)
      withMVar cfgConn $ \conn -> persistPayIndex conn index
      waitInvoices 0 index cfg


getLightningConfig :: SocketConfig -> IO LightningConfig
getLightningConfig cfg = do
  mlncfg <- timeout (5 * 1000000) (rpc_ cfg "getinfo")
  lncfg <- maybe timeouterr return mlncfg
  logError $ "[ln] using peer " ++ T.unpack (showLnPeer lncfg)
  return lncfg
  where
    timeouterr = fail "timeout during clightning getinfo call"


parseNetworkValue :: Value -> Parser BitcoinNetwork
parseNetworkValue = either fail return . getNetwork

getNetwork :: Value -> Either String BitcoinNetwork
getNetwork val = do
  network <- key "network" val
  maybe err return (parseNetwork network)
  where
    err = Left "getinfo: network key not found"
    key str (Object obj) =
      case Map.lookup str obj of
        Just (String txt) -> return txt
        Just _            -> err
        Nothing           -> err
    key _ _ = err

getIsProd :: IO Bool
getIsProd = isJust <$> lookupEnv "PRODUCTION"


getPort :: IO Int
getPort = do
  mstrport <- lookupEnv "PORT"
  return (fromMaybe 8002 (mstrport >>= readMaybe))

showSiteConfig :: Site -> Text
showSiteConfig Site{..} =
  getProtocol siteProtocol <> "://" <> getHost siteHostName


getConfig :: IO Config
getConfig = do
  port <- getPort
  socketCfg <- getSocketConfig
  isProd <- getIsProd
  lncfg <- getLightningConfig socketCfg
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
