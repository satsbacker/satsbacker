{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Lightning.Bolt11
    ( Bolt11(..)
    , bolt11Msats
    ) where

import Bitcoin.Bech32 (bech32Decode, toBase256, Word5(..))
import Bitcoin.Denomination (btc, MSats, Denomination(toMsats))

import Control.Applicative
import Data.Text (Text)
import Data.Bits ((.|.), shiftL)
import Data.Foldable (foldl')
import Data.Word (Word8)
import Data.Attoparsec.Text
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
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
    deriving (Show)

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
    _   -> fail "unhandled case in parseMultiplier"

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

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

w5int :: [Word5] -> Int
w5int bytes = foldl' decodeInt 0 (zip [0..] (Prelude.take 7 bytes))
  where
    decodeInt !n (i, UnsafeWord5 byte) = n .|. fromIntegral byte `shiftL` (i * 5)

decodeBolt11 :: Text -> Either String Bolt11
decodeBolt11 txt = do
  (hrp, w5s) <- maybeToRight "error decoding bech32" (bech32Decode txt)
  let (timestampBits, rest) = splitAt 7 w5s
      timestamp = w5int (reverse timestampBits)
      sig       = toBase256 rest
  parsedHrp <- parseOnly hrpParser hrp
  Right (Bolt11 parsedHrp timestamp (B8.pack sig))
     -- bytes      <- BS.pack <$> toBase256 w5s


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
