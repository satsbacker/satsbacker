{-# LANGUAGE RecordWildCards #-}

module Invoicing
    ( invoiceRoutes
    ) where

import Control.Applicative (optional)
import Data.Text.Encoding (decodeUtf8)
import Network.RPC.Config (SocketConfig(..))
import Web.Scotty
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8

import Bitsbacker.UniqueId (newUniqueId, encodeUniqueId)
import Bitsbacker.Config

import Bitcoin.Denomination
import Network.RPC.CLightning.Invoice
import Network.RPC.CLightning (waitinvoice)
import Network.RPC (rpc)

newInvoice :: SocketConfig -> MSats -> Text -> IO Invoice
newInvoice cfg (MSats int) description = do
  invId <- liftIO newUniqueId
  let label = encodeUniqueId invId
      args  = [show int, B8.unpack label, T.unpack description]
  newInv <- rpc cfg "invoice" args
  either fail return (fromNewInvoice (decodeUtf8 label) newInv)


getInvoice :: Config -> ActionM ()
getInvoice Config{..} = do
  msatoshis   <- msats <$> param "msatoshi"
  description <-           param "description"
  inv <- liftIO (newInvoice cfgRPC msatoshis description)
  json inv


waitInvoice :: Config -> ActionM ()
waitInvoice Config{..} = do
  mtimeout <- optional (param "timeout")
  invId <- param "invId"
  let timeout = maybe 30 (min (1800 :: Int)) mtimeout
  inv <- waitinvoice cfgRPC invId
  json inv

invoiceRoutes :: Config -> ScottyM ()
invoiceRoutes cfg = do
  post "/invoice" (getInvoice cfg)
  get "/invoice/:invId/wait" (waitInvoice cfg)

