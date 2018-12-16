{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.RPC.CLightning.Invoice
    ( Invoice(..)
    , NewInvoice(..)
    , CLInvoice(..)
    , fromNewInvoice
    ) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import Data.Aeson.Types (Parser)

import qualified Data.Vector as V
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Denomination (MSats(..))
import Network.Lightning.Bolt11

newtype NewInvoice = NewInvoice { getNewInvoice :: Text }
    deriving (Show)

instance FromJSON NewInvoice where
    parseJSON (Object obj) = NewInvoice <$> obj .: "bolt11"
    parseJSON _            = fail "NewInvoice: expected bolt11 field"

instance ToJSON Invoice where
    toJSON Invoice{..} =
        object $ [ "id"          .= invoiceId
                 , "rhash"       .= invoicePaymentHash
                 , "payreq"      .= invoicePaymentRequest
                 , "status"      .= invoiceStatus
                 , "description" .= invoiceDescription
                 , "expires_at"  .= invoiceExpires
                 , "timestamp"   .= invoiceTimestamp
                 ] ++ maybe [] (:[])
                        ((\(MSats msat) -> "msatoshi" .= msat) <$> invoiceMSat)

defaultExpiry :: Int
defaultExpiry = 3600

data Invoice = Invoice {
      invoiceId             :: Text
    , invoicePaymentHash    :: Text
    , invoicePaymentRequest :: Text
    , invoiceMSat           :: Maybe MSats
    , invoiceStatus         :: Text
    , invoiceDescription    :: Text
    , invoiceExpires        :: Int
    , invoiceTimestamp      :: Int
    }
    deriving Show

newtype CLInvoice = CLInvoice { getCLInvoice :: Invoice }
    deriving Show

defaultInvoice :: Invoice
defaultInvoice =
    Invoice {
      invoiceId             = ""
    , invoicePaymentHash    = ""
    , invoicePaymentRequest = ""
    , invoiceMSat           = Nothing
    , invoiceStatus         = "unpaid"
    , invoiceDescription    = ""
    , invoiceExpires        = defaultExpiry
    , invoiceTimestamp      = 0
    }

fromNewInvoice :: Text -> NewInvoice -> Either String Invoice
fromNewInvoice label (NewInvoice bolt11) = do
  decoded <- decodeBolt11 bolt11
  return (fromBolt11 label bolt11 decoded)

fromBolt11 :: Text -> Text -> Bolt11 -> Invoice
fromBolt11 invId rawBolt11 Bolt11{..} =
  foldr update initInvoice bolt11Tags
  where
    initInvoice =
        defaultInvoice {
          invoiceId             = invId
        , invoicePaymentRequest = rawBolt11
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
        Description txt -> i { invoiceDescription = txt }
        Expiry t        -> i { invoiceExpires = t }
        _               -> i
        -- TODO: more invoice data from bolt11


parseCLInvoice :: Object -> Parser Invoice
parseCLInvoice obj =
    Invoice <$> obj .: "label"
            <*> obj .: "payment_hash"
            <*> obj .: "bolt11"
            <*> (fmap (fmap MSats) (obj .:? "msatoshi"))
            <*> obj .: "status"
            <*> obj .: "description"
            <*> obj .: "expires_at"
            <*> pure defaultExpiry

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
