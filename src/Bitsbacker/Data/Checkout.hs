{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bitsbacker.Data.Checkout
    ( CheckoutPage(..)
    , mkCheckoutPage
    , describeCheckoutError
    , CheckoutError(..)
    ) where

import Control.Concurrent.MVar (withMVar, MVar(..))
import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Database.SQLite.Simple (Connection)
import System.IO (stderr, hPutStrLn)

import Bitsbacker.Data.Tiers
import Bitsbacker.Data.User
import Bitsbacker.Config
import Invoicing
import Bitcoin.Denomination (MSats, msats, toBits, showBits)

import Network.RPC.CLightning.Invoice
import Network.RPC.Config (SocketConfig(..))

import Data.Text (Text)
import qualified Data.Text as T

data CheckoutPage = CheckoutPage
  { checkoutTier    :: Tier
  , checkoutInvoice :: Invoice
  , checkoutUser    :: User
  }

data CheckoutError =
    InvoiceCreationFailed String
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
      InvalidTier _ -> "The selected tier is unavailable"

logErr e = hPutStrLn stderr e


safeGetTierById :: MVar Connection -> TierId -> IO (Either CheckoutError Tier)
safeGetTierById mvconn tierId = do
  etier_ :: Either SomeException (Maybe Tier) <-
              try $ withMVar mvconn (\conn -> getTierById conn tierId)
  let etier = do mtier <- either (Left . InvalidTier . show) Right etier_
                 maybe (Left (InvalidTier "missing")) Right mtier
  return etier


safeGetInvoice :: MVar SocketConfig -> MSats -> Text
               -> IO (Either CheckoutError Invoice)
safeGetInvoice cfgRPC msat desc = do
  einvoice :: Either SomeException Invoice <-
                try $ withMVar cfgRPC $ \cfg -> newInvoice cfg msat desc
  return $
    case einvoice of
      Left err  -> Left (InvoiceCreationFailed (show err))
      Right inv -> Right inv

-- TODO: denominationdisplay
mkInvoiceDesc :: Username -> MSats -> Text -> Text
mkInvoiceDesc (Username name) msats desc =
    "Back " <> name <> " at " <> T.pack amountBits <> " bits per month: " <> desc
    where
      amountBits = showBits (toBits msats) 

mkCheckoutPage :: Config -> TierId -> IO (Either CheckoutError CheckoutPage)
mkCheckoutPage Config{..} tierId = do
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
      einvoice <- safeGetInvoice cfgRPC msat invDesc
      return $
        case einvoice of
          Left  err -> Left err
          Right inv -> Right (CheckoutPage
                                { checkoutTier = tier
                                , checkoutInvoice = inv
                                , checkoutUser = user
                                })
