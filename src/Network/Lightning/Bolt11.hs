{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Lightning.Bolt11
    ( Bolt11(..)
    , bolt11Msats
    ) where

import Bitcoin.Bech32 (bech32Decode, toBase256, HRP)
import Bitcoin.Denomination (btc, MSats, Denomination(toMsats))

import Data.Text (Text)
import Data.Aeson
import Data.Serialize.Get


import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
-- import qualified Crypto.Secp256k1 as Secp

data Multiplier = Milli | Micro | Nano | Pico
                deriving (Show, Eq, Ord)

data Bolt11HRP = Bolt11HRP {
      bolt11Prefix      :: Text
    , bolt11Multiplier  :: Maybe Multiplier
    , bolt11Amount      :: Int
    }

data Bolt11 = Bolt11 {
      bolt11HRP       :: Bolt11HRP 
    , bolt11Timestamp :: Int -- posix
    , bolt11Signature :: ByteString
    }

-- instance FromJSON Bolt11 where
--     parseJSON (String txt) = maybe (fail "could not decode bolt11") return
--                                    (decodeBolt11 txt)
--     parseJSON _            = fail "Expected a string when parsing Bolt11"

-- decodeBolt11 :: Text -> Maybe Bolt11
-- decodeBolt11 txt = decodeBolt11ToBs txt >>= uncurry decodeBolt11BS

-- bolt11Parser :: HRP -> Get Bolt11
-- bolt11Parser hrp = do

decodeBolt11ToBs :: Text -> Maybe (HRP, ByteString)
decodeBolt11ToBs txt = do
  (hrp, w5s) <- bech32Decode txt
  w8s <- toBase256 w5s
  return (hrp, BS.pack w8s)


multiplierRatio :: Multiplier -> Rational
multiplierRatio m =
    case m of
      Milli -> 1 / 1000
      Micro -> 1 / 1000000
      Nano  -> 1 / 1000000000
      Pico  -> 1 / 1000000000000

bolt11Msats :: Bolt11HRP -> MSats
bolt11Msats Bolt11HRP{..} =
    let
        multi  = maybe 1 multiplierRatio bolt11Multiplier
        btcAmt = multi * toRational bolt11Amount
    in
        toMsats (btc btcAmt)

