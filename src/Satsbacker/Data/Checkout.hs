{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Satsbacker.Data.Checkout
    ( CheckoutPage(..)
    , getCheckoutPage
    , mkCheckout
    , describeCheckoutError
    , checkoutPageToJSON
    , CheckoutError(..)
    ) where

import Control.Concurrent.MVar (withMVar, MVar)
import Control.Exception (try, SomeException)
import Data.Aeson
import Data.String (IsString)
import Database.SQLite.Simple (Connection)

import Bitcoin.Denomination (MSats)
import Invoicing
import Satsbacker.AmountConfig
import Satsbacker.Config
import Satsbacker.Data.Invoice
import Satsbacker.Data.Site (Site(..))
import Satsbacker.Data.InvoiceId (InvId(..))
import Satsbacker.Data.Tiers
import Satsbacker.Data.User

import Network.RPC.Config (SocketConfig(..))
import Network.RPC.CLightning (listinvoices)

import Data.Text (Text)

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

checkoutPageToJSON :: AmountConfig -> CheckoutPage -> Value
checkoutPageToJSON acfg CheckoutPage{..} =
  object [ "tier"    .= tierToJSON acfg checkoutTier
         , "invoice" .= invoiceToJSON acfg checkoutInvoice
         , "user"    .= checkoutUser
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
mkInvoiceDesc :: AmountConfig -> Username -> MSats -> Text -> Text
mkInvoiceDesc acfg (Username name) msat desc =
    "Back " <> name <> " at " <> amount
            <> " " <> denom <> " per month: " <> desc
    where
      amount = renderAmount acfg msat
      denom  = renderDenomination acfg

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
                try $ listinvoices cfgRPC (getInvId invRefInvoiceId)
      return $
        case invs of
          Left err    -> Left $ InvoiceFetchFailed (show err)
          Right []    -> Left $ InvoiceFetchFailed "missing"
          Right [inv] -> Right $ CheckoutPage
                                { checkoutTier = tier
                                , checkoutInvoice = inv
                                , checkoutUser = user
                                }
          Right _ ->
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
          acfg    = siteAmountCfg cfgSite
          invDesc = mkInvoiceDesc acfg backee msat tdesc
      safeNewInvoice cfgRPC msat invDesc
