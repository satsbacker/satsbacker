
module Bitsbacker.InvoiceId where

import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Base58
import Data.ByteString (ByteString)

newtype InvoiceId = InvoiceId { getNanoUuid :: UUID }

instance Show InvoiceId where
  show invoiceId = BS.unpack (encodeInvoiceId invoiceId)

encodeInvoiceId :: InvoiceId -> ByteString
encodeInvoiceId (InvoiceId uuid) =
    let
        uuidBytes = LBS.toStrict (UUID.toByteString uuid)
    in
        encodeBase58 bitcoinAlphabet uuidBytes

newInvoiceId :: IO InvoiceId
newInvoiceId = fmap InvoiceId UUID.nextRandom
