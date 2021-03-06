{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Data.Invoice
    ( Invoice(..)
    , NewInvoice(..)
    , InvoiceRef(..)
    , WaitInvoice(..)
    , CLInvoice(..)
    , CLInvoices(..)
    , fromNewInvoice
    , invoiceToJSON
    , isPaid
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple

import qualified Data.Vector as V
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL

import Satsbacker.AmountConfig
import Satsbacker.Data.Email
import Satsbacker.Data.InvoiceId (InvId(..))
import Satsbacker.Data.Tiers (TierId)
import Satsbacker.Data.User (UserId(..))
import Database.SQLite.Table

import Bitcoin.Denomination (MSats(..))
import Network.Lightning.Bolt11


infixr 5 ?:
(?:) :: Maybe a -> [a] -> [a]
Just x  ?: xs = x : xs
Nothing ?: xs = xs

newtype WaitInvoice = WaitInvoice { getWaitInvoice :: (Int, CLInvoice) }
    deriving Show

newtype NewInvoice = NewInvoice { getNewInvoice :: Text }
    deriving Show

data InvoiceRef = InvoiceRef
    { invRefInvoiceId :: InvId
    , invRefTierId    :: TierId
    , invRefEmail     :: Email
    , invRefPayerId   :: Maybe UserId
    }
    deriving Show

instance FromJSON WaitInvoice where
    parseJSON v@(Object obj) = fmap WaitInvoice $
        (,) <$> obj .: "pay_index"
            <*> parseJSON v
    parseJSON _ = fail "expected object when parsing WaitInvoice"

instance FromJSON NewInvoice where
    parseJSON (Object obj) = NewInvoice <$> obj .: "bolt11"
    parseJSON _            = fail "NewInvoice: expected bolt11 field"

instance ToRow InvoiceRef where
    toRow invRef =
        let
            InvoiceRef f1 f2 f3 f4 = invRef
        in
          toRow ( getInvId f1
                , f2
                , f3
                , fmap getUserId f4
                )

invoiceRefFields :: [Text]
invoiceRefFields =
    [ "invoice_id"
    , "tier_id"
    , "email"
    , "payer_id"
    ]

guardEmpty :: (Eq t, Monoid t) => (t -> a) -> t -> Maybe a
guardEmpty _ v | v == mempty = Nothing
guardEmpty f txt             = Just (f txt)


invoiceToJSON :: AmountConfig -> Invoice -> Value
invoiceToJSON acfg Invoice{..} =
  object $
    ((\(MSats msat) -> "msatoshi" .= msat) <$> invoiceMSat) ?:
    ((\msat -> "amount" .= renderAmount acfg msat) <$> invoiceMSat) ?:
    (fmap ("paid_at" .=) invoicePaidAt) ?:
    (guardEmpty ("description" .=) =<< invoiceDescription) ?:
    (guardEmpty ("payreq" .=) =<< invoicePaymentRequest) ?:
    [ "id"          .= invoiceId
    , "rhash"       .= invoicePaymentHash
    , "status"      .= invoiceStatus
    , "expiry"      .= invoiceExpires
    , "expires_at"  .= invoiceExpires
        -- FIXME: this should be invoiceExpires + invoiceTimestamp
    , "timestamp"   .= invoiceTimestamp
    ]


-- renderInvoice :: DisplayConfig -> Value
-- renderInvoice = error "implement me"

defaultExpiry :: Int
defaultExpiry = 3600

data Invoice = Invoice {
      invoiceId             :: InvId
    , invoicePaymentHash    :: Text
    , invoicePaymentRequest :: Maybe Text
    , invoiceMSat           :: Maybe MSats
    , invoiceStatus         :: Text
    , invoiceDescription    :: Maybe Text
    , invoiceExpires        :: Int
    , invoicePaidAt         :: Maybe Int
    , invoiceTimestamp      :: Int
    }
    deriving Show

isPaid :: Invoice -> Bool
isPaid = isJust . invoicePaidAt

instance Table InvoiceRef where
    tableName _ = "invoices"
    tableFields _ = invoiceRefFields

instance FromRow InvoiceRef where
    fromRow =
        InvoiceRef <$> fmap InvId field
                   <*> field
                   <*> field
                   <*> fmap (fmap UserId) field

newtype CLInvoice = CLInvoice { getCLInvoice :: Invoice }
    deriving Show

newtype CLInvoices = CLInvoices { getCLInvoices :: [Invoice] }
    deriving Show

defaultInvoice :: Invoice
defaultInvoice =
    Invoice {
      invoiceId             = InvId ""
    , invoicePaymentHash    = ""
    , invoicePaymentRequest = Nothing
    , invoiceMSat           = Nothing
    , invoiceStatus         = "unpaid"
    , invoiceDescription    = Nothing
    , invoiceExpires        = defaultExpiry
    , invoicePaidAt         = Nothing
    , invoiceTimestamp      = 0
    }

fromNewInvoice :: InvId -> NewInvoice -> Either String Invoice
fromNewInvoice invId (NewInvoice bolt11) = do
  decoded <- decodeBolt11 bolt11
  return (fromBolt11 invId bolt11 decoded)

fromBolt11 :: InvId -> Text -> Bolt11 -> Invoice
fromBolt11 invId rawBolt11 Bolt11{..} =
  foldr update initInvoice bolt11Tags
  where
    initInvoice =
        defaultInvoice {
          invoiceId             = invId
        , invoicePaymentRequest = Just rawBolt11
        , invoiceMSat           = fmap bolt11Msats (bolt11Amount bolt11HRP)
        , invoiceTimestamp      = bolt11Timestamp
        }

    update tag !i =
      case tag of
        PaymentHash (Hex bs) ->
            let
                hexStr = BS.toLazyByteString (BS.byteStringHex bs)
            in
                i { invoicePaymentHash = decodeUtf8 (BL.toStrict hexStr) }
        Description txt -> i { invoiceDescription = Just txt }
        Expiry t        -> i { invoiceExpires = t }
        _               -> i
        -- TODO: more invoice data from bolt11


parseCLInvoice :: Object -> Parser Invoice
parseCLInvoice obj =
    Invoice <$> fmap InvId (obj .: "label")
            <*> obj .: "payment_hash"
            <*> obj .:? "bolt11"
            <*> (fmap (fmap MSats) (obj .:? "msatoshi"))
            <*> obj .: "status"
            <*> obj .:? "description"
            <*> obj .: "expires_at" -- FIXME: need to convert to expiry
            <*> obj .:? "paid_at"
            <*> pure 0 -- FIXME: need to populate timestamp somehow

instance FromJSON CLInvoices where
    parseJSON (Object obj) =
        CLInvoices <$> (fmap (map getCLInvoice) (obj .: "invoices"))
    parseJSON _            = fail "unspected listinvoices value"

instance FromJSON CLInvoice where
    parseJSON (Array a)
      | V.length a == 0 = fail "could not find invoice"
      | V.length a > 1  = fail "ambiguous invoice id"
      | otherwise =
          case V.head a of
            Object obj -> CLInvoice <$> parseCLInvoice obj
            _          -> fail "unknown clightning invoice encoding"
    parseJSON (Object obj) = CLInvoice <$> parseCLInvoice obj
    parseJSON _            = fail "unspected CLInvoice value"
