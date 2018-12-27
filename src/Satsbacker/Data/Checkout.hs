{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Satsbacker.Data.Checkout
    ( CheckoutPage(..)
    , getCheckoutPage
    , mkCheckout
    , describeCheckoutError
    , CheckoutError(..)
    ) where

import Control.Concurrent.MVar (withMVar, MVar)
import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Maybe (listToMaybe)
import Data.String (IsString)
import Database.SQLite.Simple (Connection)

import Satsbacker.Data.Tiers
import Satsbacker.Data.User
import Satsbacker.Config
import Satsbacker.Data.Invoice
import Invoicing
import Bitcoin.Denomination (MSats, toBits, showBits)

import Network.RPC.Config (SocketConfig(..))
import Network.RPC.CLightning (listinvoices)

import Data.Text (Text)
import qualified Data.Text as T

data CheckoutPage = CheckoutPage
  { checkoutTier    :: Tier
  , checkoutInvoice :: Invoice
  , checkoutUser    :: User
  }

data CheckoutError =
    InvoiceCreationFailed String
  | InvoiceFetchFailed String
  | InvalidTier String
  deriving Show

instance ToJSON CheckoutPage where
    toJSON CheckoutPage{..} =
        object [ "tier"    .= checkoutTier
               , "invoice" .= checkoutInvoice
               , "user" .= checkoutUser
               ]

describeCheckoutError :: IsString p => CheckoutError -> p
describeCheckoutError checkoutErr =
    case checkoutErr of
      InvoiceCreationFailed _ -> "There was an error creating the invoice"
      InvoiceFetchFailed _ -> "There was an error retrieving the invoice"
      InvalidTier _ -> "The selected tier is unavailable"

safeGetTierById :: MVar Connection -> TierId -> IO (Either CheckoutError Tier)
safeGetTierById mvconn tierId = do
  etier_ :: Either SomeException (Maybe Tier) <-
              try $ withMVar mvconn (\conn -> getTierById conn tierId)
  let etier = do mtier <- either (Left . InvalidTier . show) Right etier_
                 maybe (Left (InvalidTier "missing")) Right mtier
  return etier


safeNewInvoice :: SocketConfig -> MSats -> Text
               -> IO (Either CheckoutError Invoice)
safeNewInvoice cfgRPC msat desc = do
  einvoice :: Either SomeException Invoice <-
                try $ newInvoice cfgRPC msat desc
  return $
    case einvoice of
      Left err  -> Left (InvoiceCreationFailed (show err))
      Right inv -> Right inv

-- TODO: denominationdisplay
mkInvoiceDesc :: Username -> MSats -> Text -> Text
mkInvoiceDesc (Username name) msat desc =
    "Back " <> name <> " at " <> T.pack amountBits <> " bits per month: " <> desc
    where
      amountBits = showBits (toBits msat) 

getCheckoutPage :: Config -> InvoiceRef -> IO (Either CheckoutError CheckoutPage)
getCheckoutPage Config{..} InvoiceRef{..} = do
  etier <- safeGetTierById cfgConn invRefTierId
  case etier of
    Left err  -> return (Left err)
    Right tier -> do
      let tdef = tierDef tier
          uid  = tierUserId tdef
      muser <- withMVar cfgConn $ \conn -> getUserById conn uid
      user  <- maybe (fail $ "missing user " ++ show uid) return muser
      invs :: Either SomeException [Invoice] <-
                try $ listinvoices cfgRPC invRefInvoiceId
      return $
        case invs of
          Left err    -> Left $ InvoiceFetchFailed (show err)
          Right []    -> Left $ InvoiceFetchFailed "missing"
          Right [inv] -> Right $ CheckoutPage
                                { checkoutTier = tier
                                , checkoutInvoice = inv
                                , checkoutUser = user
                                }
          Right more ->
            Left $ InvoiceFetchFailed "more than one invoice with the same id"

mkCheckout :: Config -> TierId -> IO (Either CheckoutError Invoice)
mkCheckout Config{..} tierId = do 
  etier <- safeGetTierById cfgConn tierId
  case etier of
    Left err  -> return (Left err)
    Right tier -> do
      let tdef = tierDef tier
          uid  = tierUserId tdef
      muser    <- withMVar cfgConn $ \conn -> getUserById conn uid
      user     <- maybe (fail $ "missing user " ++ show uid) return muser
      let msat    = tierAmountMSats tdef
          tdesc   = tierDescription tdef
          backee  = userName user
          invDesc = mkInvoiceDesc backee msat tdesc
      safeNewInvoice cfgRPC msat invDesc
