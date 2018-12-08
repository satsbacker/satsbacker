
{-# LANGUAGE OverloadedStrings #-}

module Network.RPC.CLightning.Output
    ( Output(..)
    , ListFundsResp(..)
    ) where

import Data.Aeson
import Data.Text (Text)
import Data.Word (Word16)

data Output = Output {
      outputTxid  :: Text
    , outputIndex :: !Word16
    , outputValue :: Int
    } deriving (Show, Eq)

data ListFundsResp = ListFundsResp { listFundsOutputs :: [Output] }
                   deriving (Show, Eq)

instance FromJSON Output where
  parseJSON (Object obj) =
    Output
      <$> obj .: "txid"
      <*> obj .: "output"
      <*> obj .: "value"
  parseJSON _ = fail "Output is not an object"

instance FromJSON ListFundsResp where
  parseJSON (Object obj) =
      ListFundsResp <$> obj .: "outputs"
  parseJSON _ = fail "ListFundsResp is not an object"
