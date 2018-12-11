{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Lightning.Bolt11
    ( Bolt11(..)
    , bolt11Msats
    ) where

import Bitcoin.Bech32 (bech32Decode, toBase256)
import Bitcoin.Denomination (btc, MSats, Denomination(toMsats))

import Control.Applicative
import Data.Text (Text)
import Data.Attoparsec.Text
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
-- import qualified Crypto.Secp256k1 as Secp

data Multiplier = Milli | Micro | Nano | Pico
                deriving (Show, Eq, Ord)

data Currency = Bitcoin
              | BitcoinTestnet
              | BitcoinRegtest
              deriving (Show, Eq)

data Bolt11HRP = Bolt11HRP {
      bolt11Currency    :: Currency
    , bolt11Amount      :: Maybe (Int, Multiplier)
    }
    deriving (Show)

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

parseCurrency :: Parser Currency
parseCurrency =
      (string "bc"   *> pure Bitcoin)
  <|> (string "tb"   *> pure BitcoinTestnet)
  <|> (string "bcrt" *> pure BitcoinRegtest) 

parseMultiplier :: Parser Multiplier
parseMultiplier = do
    c <- satisfy (`elem` ("munp" :: String))
    case c of
      'm' -> pure Milli
      'u' -> pure Micro
      'n' -> pure Nano
      'p' -> pure Pico
      _   -> error "impossible"

parseHrpAmount :: Parser (Int, Multiplier)
parseHrpAmount = do
  amt   <- decimal
  multi <- parseMultiplier
  return (amt, multi)

hrpParser :: Parser Bolt11HRP
hrpParser = do
  char 'l'
  char 'n'
  currency <- parseCurrency
  mamt     <- optional parseHrpAmount
  return (Bolt11HRP currency mamt)

decodeBolt11 :: Text -> Either String Bolt11HRP
decodeBolt11 txt =
  let mhrp = do (hrp, w5s) <- bech32Decode txt
                return hrp
                -- bytes      <- BS.pack <$> toBase256 w5s
  in
    case mhrp of
      Nothing   -> Left "no hrp found in bolt11"
      Just hrp -> parseOnly hrpParser hrp


multiplierRatio :: Multiplier -> Rational
multiplierRatio m =
    case m of
      Milli -> 1 / 1000
      Micro -> 1 / 1000000
      Nano  -> 1 / 1000000000
      Pico  -> 1 / 1000000000000

bolt11Msats :: Bolt11HRP -> Maybe MSats
bolt11Msats  Bolt11HRP{..} = do
  (amt, multi) <- bolt11Amount
  return (toMsats (btc (multiplierRatio multi * toRational amt)))

