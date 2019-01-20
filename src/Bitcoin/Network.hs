{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network
    ( BitcoinNetwork(..)
    , parseNetwork
    ) where

import Data.Text (Text)
import Data.Aeson

data BitcoinNetwork = Mainnet
                    | Testnet
                    | Regtest
                    deriving (Eq, Ord, Enum)

instance Show BitcoinNetwork where
    show Mainnet = "mainnet"
    show Testnet = "testnet"
    show Regtest = "regtest"

instance ToJSON BitcoinNetwork where
    toJSON Mainnet = String "mainnet"
    toJSON Testnet = String "testnet"
    toJSON Regtest = String "regtest"

parseNetwork :: Text -> Maybe BitcoinNetwork
parseNetwork "bitcoin" = Just Mainnet
parseNetwork "mainnet" = Just Mainnet
parseNetwork "testnet" = Just Testnet
parseNetwork _         = Nothing
