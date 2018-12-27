{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Invoicing
    ( invoiceRoutes
    , newInvoice
    , waitInvoices
    ) where

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative (optional)
import Control.Concurrent
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Base (RequestMethod(GET))
import Network.HTTP.Types (status402, status500)
import Network.Wai (Middleware, pathInfo, requestMethod)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import System.Timeout (timeout)
import Web.Scotty


import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8

import Satsbacker.Config
import Satsbacker.Logging
import Satsbacker.Data.Invoice
import Satsbacker.InvoiceId (newInvoiceId, encodeInvoiceId)
import Network.RPC.CLightning (listinvoices)
import Network.RPC.Config (SocketConfig(..))

import Bitcoin.Denomination


import Network.RPC (rpc)

newInvoice :: SocketConfig -> MSats -> Text -> IO Invoice
newInvoice cfg (MSats int) description = do
  invId <- liftIO newInvoiceId
  let label = encodeInvoiceId invId
      args  = [show int, B8.unpack label, T.unpack description]
  newInv <- rpc cfg "invoice" args
  let inv = fromNewInvoice (decodeUtf8 label) newInv
  either fail return inv

postInvoice :: Config -> ActionM ()
postInvoice Config{..} = do
  msatoshis   <- msats <$> param "msatoshi"
  description <-           param "description"
  inv <- liftIO (newInvoice cfgRPC msatoshis description)
  json inv


micro :: Int
micro = 1000000

-- max 30 minutes
sanitizeTimeout :: Int -> Int
sanitizeTimeout = min (1800 * micro) . (*micro)


waitForInvoice :: MVar WaitInvoice -> Text -> IO Invoice
waitForInvoice !mv !invId = do
  WaitInvoice (!_, !(CLInvoice !inv)) <- takeMVar mv
  if (invoiceId inv == invId)
     then return inv
     else waitForInvoice mv invId

checkPaidInvoice :: SocketConfig -> Text -> IO (Maybe Invoice)
checkPaidInvoice cfgRPC invId = do
  invs <- listinvoices cfgRPC invId
  case invs of
    []    -> return Nothing
    [inv] -> if isPaid inv
                then return (Just inv)
                else return Nothing
    _ -> fail ("more than one invoice with id "  ++ T.unpack invId)

pinger stop events = do
  m <- tryTakeMVar stop
  case m of
    Just () -> return ()
    Nothing -> do
      writeChan events commentEvent
      threadDelay (micro * 15)
      pinger stop events
  where
    commentEvent = CommentEvent (fromByteString "ping")


waitInvoice :: Config -> Chan ServerEvent -> Maybe Int -> Text -> IO ()
waitInvoice Config{..} events mtimeout invId = do
  mpaid <- checkPaidInvoice cfgRPC invId
  stop <- newEmptyMVar
  case mpaid of
    Nothing  -> wait stop mtimeout invId
    Just inv -> sendPaidEvent stop
  where
    eventName     = fromByteString "paid"
    eventId       = Just (fromByteString "0")
    paidEvent     = ServerEvent (Just eventName) eventId [eventName]
    sendPaidEvent stop = do
      putMVar stop ()
      writeChan events paidEvent
      writeChan events CloseEvent

    wait pingStop mtimeout invId = do
      -- default 30 second timeout
      let waitFor = maybe (30 * micro) sanitizeTimeout mtimeout
      forkIO (pinger pingStop events)

      minv <- timeout waitFor $ waitForInvoice cfgPayNotify invId
      case minv of
        Nothing  -> do putMVar pingStop ()
                       writeChan events CloseEvent
        Just inv -> sendPaidEvent pingStop

waitinvoiceSSE :: Config -> Middleware
waitinvoiceSSE config next req responder =
    let
        method   = requestMethod req
        path     = pathInfo req
        mtimeout = Just 600 -- 10 minutes
    in
      case (method, path) of
        ("GET", ["payment-stream", invId]) -> do
            events <- newChan
            forkIO (waitInvoice config events mtimeout invId)
            let sseApp = eventSourceAppChan events
            sseApp req responder
        (_, _) ->
            next req responder


invoiceRoutes :: Config -> ScottyM ()
invoiceRoutes cfg = do
  post "/invoice" (postInvoice cfg)
  -- get "/invoice/:invId/wait" (waitInvoice cfg)
  middleware (waitinvoiceSSE cfg)
