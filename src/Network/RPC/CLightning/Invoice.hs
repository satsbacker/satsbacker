
module Network.RPC.CLightning.Invoice
    ( Invoice(..)
    ) where

import Data.Text (Text)

import Network.Lightning.Bolt11 (Bolt11)

data Invoice = Invoice {
      invoiceBolt11      :: Bolt11
    , invoicePaymentHash :: Text
    , invoiceExpires     :: Int
    }
