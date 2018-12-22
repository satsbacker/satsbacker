{-# LANGUAGE RecordWildCards #-}

module Invoicing
    ( invoiceRoutes
    , newInvoice
    ) where

import Control.Applicative (optional)
import Data.Text.Encoding (decodeUtf8)
import Network.RPC.Config (SocketConfig(..))
import Control.Concurrent.MVar (withMVar)
import Network.HTTP.Types (status402)
import Web.Scotty
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import System.Timeout (timeout)

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
  inv <- liftIO $ withMVar cfgRPC $ \sock ->
           newInvoice sock msatoshis description
  json inv

micro :: Int
micro = 1000000

-- max 30 minutes
sanitizeTimeout :: Int -> Int
sanitizeTimeout = min (1800 * micro) . (*micro)

waitInvoice :: Config -> ActionM ()
waitInvoice Config{..} = do
  mtimeout <- optional (param "timeout")
  invId <- param "invId"
  -- default 30 second timeout
  let waitFor = maybe (30 * micro) sanitizeTimeout mtimeout
  minv <- liftIO $ timeout waitFor $ withMVar cfgRPC $ \sock ->
            waitinvoice sock invId
  case minv of
    Nothing  -> status status402
    Just inv -> json inv

invoiceRoutes :: Config -> ScottyM ()
invoiceRoutes cfg = do
  post "/invoice" (getInvoice cfg)
  get "/invoice/:invId/wait" (waitInvoice cfg)

