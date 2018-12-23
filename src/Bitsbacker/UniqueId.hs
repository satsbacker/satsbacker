{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Bitsbacker.InvoiceId where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Char (chr, ord)
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.String (fromString)
import Data.Word (Word8)
import Numeric (readInt, showIntAtBase)
import System.Entropy (getEntropy)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BUIDL
import qualified Data.ByteString.Char8 as B8

newtype InvoiceId = InvoiceId { getInvoiceId :: Int64 }

instance Show InvoiceId where
  show invId = B8.unpack (encodeInvoiceId invId)

encodeInvoiceId :: InvoiceId -> ByteString
encodeInvoiceId (InvoiceId uuid) =
  invoiceIdEncode uuidBytes
  where
    uuidBytes = LBS.toStrict (BUIDL.toLazyByteString (BUIDL.int64BE uuid))

randInt :: IO Int64
randInt = do
    bs <- getEntropy 8
    return (w8int64 bs)

w8int64 :: ByteString -> Int64
w8int64 bytes = foldl' decodeInt 0 (BS.zip "\0\1\2\3\4\5\6\7\8" bytes)
  where
    decodeInt !n (i, byte) =
        n .|. fromIntegral byte `shiftL` (fromIntegral i * 8)

newInvoiceId :: IO InvoiceId
newInvoiceId = fmap InvoiceId randInt

-- λ> replicateM 5 newInvoiceId >>= mapM_ print
-- C5YYNAS2UPGPR
-- N7XMRMMMQY3SM
-- CNUWBCP3QZ7DU
-- KLP5QWNBMR2P6
-- DQWK3M2XMHZLV

table :: BS.ByteString
table = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"

invoiceId :: Word8 -> Word8
invoiceId i = BS.index table (fromIntegral i)

invoiceId' :: Word8 -> Maybe Word8
invoiceId' w = fromIntegral <$> BS.elemIndex w table

invoiceIdEncodeInt :: Integer
             -> BS.ByteString
invoiceIdEncodeInt i =
    fromString $ showIntAtBase (32 :: Integer) f (fromIntegral i) ""
  where
    f = chr . fromIntegral . invoiceId . fromIntegral

invoiceIdDecodeInt :: BS.ByteString
             -> Maybe Integer
invoiceIdDecodeInt s = case go of
    Just (r,[]) -> Just r
    _           -> Nothing
  where
    c = invoiceId' . fromIntegral . ord
    p = isJust . c
    f = fromIntegral . fromJust . c
    go = listToMaybe $ readInt 32 p f (B8.unpack s)

invoiceIdEncode :: BS.ByteString
          -> BS.ByteString
invoiceIdEncode input = BS.append l r
  where
    (z, b) = BS.span (== 0) input
    l = BS.map invoiceId z -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = invoiceIdEncodeInt (bsToInteger b)

invoiceIdDecode :: BS.ByteString
          -> Maybe BS.ByteString
invoiceIdDecode input = liftM (BS.append prefix) r
  where
    (z,b)  = BS.span (== invoiceId 0) input
    prefix = BS.map (fromJust . invoiceId') z -- preserve leading 1's
    r | BS.null b = Just BS.empty
      | otherwise = integerToBS <$> invoiceIdDecodeInt b

-- | Decode a big endian Integer from a bytestring
bsToInteger :: BS.ByteString -> Integer
bsToInteger = foldr f 0 . reverse . BS.unpack
  where
    f w n = toInteger w .|. shiftL n 8

-- | Encode an Integer to a bytestring as big endian
integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0     = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just (fromInteger x :: Word8, x `shiftR` 8)
