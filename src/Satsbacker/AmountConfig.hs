{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.AmountConfig
    ( AmountConfig(..)
    , renderAmount
    , renderDenomination
    ) where

import Data.Text (Text)
import Bitcoin.Denomination
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok

data AmountConfig = AmountSats
                  | AmountBits
                  | AmountBTC
                  deriving (Show, Eq, Ord)

instance ToField AmountConfig where
    toField = toField . renderDenomination

instance FromField AmountConfig where
    fromField f =
        case fromField f of
          Errors _ -> defReturn
          Ok t     -> case parseAmountConfig t of
                        Nothing -> defReturn
                        Just v  -> Ok v
        where
          defReturn = Ok AmountSats

renderAmount :: Denomination a => AmountConfig -> a -> Text
renderAmount cfg amt =
    case cfg of
      AmountSats -> showSats (toSats amt)
      AmountBits -> showBits (toBits amt)
      AmountBTC  -> showBtc  (toBtc amt)

parseAmountConfig :: Text -> Maybe AmountConfig
parseAmountConfig txt =
    case txt of
      "sats" -> Just AmountSats
      "bits" -> Just AmountBits
      "BTC"  -> Just AmountBTC
      _      -> Nothing

renderDenomination :: AmountConfig -> Text
renderDenomination AmountSats = "sats"
renderDenomination AmountBits = "bits"
renderDenomination AmountBTC  = "BTC"
