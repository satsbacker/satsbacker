
module Invoicing
    ( invoiceRoutes
    ) where

import Control.Applicative (optional)
import Data.Int (Int64)
import Database.SQLite.Simple (Connection)
import Data.Text.Encoding (decodeUtf8)
import Network.RPC.Config (SocketConfig(..))
import Web.Scotty
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8

import Bitsbacker.UniqueId (newUniqueId, encodeUniqueId)

import Bitcoin.Denomination
import Network.RPC.CLightning.Invoice
import Network.RPC (rpc)

newInvoice :: SocketConfig -> MSats -> Text -> IO Invoice
newInvoice cfg (MSats int) description = do
  invoiceId <- liftIO newUniqueId
  let label = encodeUniqueId invoiceId
      args  = [show int, B8.unpack label, T.unpack description]
  newInvoice <- rpc cfg "invoice" args
  either fail return (fromNewInvoice (decodeUtf8 label) newInvoice)


getInvoice :: Connection -> SocketConfig -> ActionM ()
getInvoice db rpc = do
  msatoshis   <- msats <$> param "msatoshi"
  description <-           param "description"
  inv <- liftIO (newInvoice rpc msatoshis description)
  json inv

invoiceRoutes conn rpc = do
  post "/invoice" (getInvoice conn rpc)

