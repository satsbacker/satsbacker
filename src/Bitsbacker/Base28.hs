{-# LANGUAGE TemplateHaskell #-}
-- base32string package

module Bitsbacker.Base28 ( b28Encode
                         , b28Decode
                         ) where

import           Control.Applicative   ((<$>))
import           Control.Monad         (liftM)

import           Data.Bits             (shiftL, shiftR, (.|.))
import           Data.Char             (chr, ord)
import           Data.List             (unfoldr)

import           Data.Maybe            (fromJust, isJust, listToMaybe)

import           Data.String           (fromString)
import           Data.Word             (Word8)
import           Numeric               (readInt, showIntAtBase)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

w82c :: Word8 -> Char
w82c = toEnum . fromIntegral

table :: BS.ByteString
table = BS.pack $ filter (not . (`elem` ("I10O" :: [Char])) . w82c) $
           [65..90]
        ++ [50..55]

b28 :: Word8 -> Word8
b28 i = BS.index table (fromIntegral i)

b28' :: Word8 -> Maybe Word8
b28' w = fromIntegral <$> BS.elemIndex w table

b28EncodeInt :: Integer
             -> BS.ByteString
b28EncodeInt i =
    fromString $ showIntAtBase (28 :: Integer) f (fromIntegral i) ""
  where
    f = chr . fromIntegral . b28 . fromIntegral

b28DecodeInt :: BS.ByteString
             -> Maybe Integer
b28DecodeInt s = case go of
    Just (r,[]) -> Just r
    _           -> Nothing
  where
    c = b28' . fromIntegral . ord
    p = isJust . c
    f = fromIntegral . fromJust . c
    go = listToMaybe $ readInt 32 p f (BS8.unpack s)

b28Encode :: BS.ByteString
          -> BS.ByteString
b28Encode input = BS.append l r
  where
    (z, b) = BS.span (== 0) input
    l = BS.map b28 z -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = b28EncodeInt (bsToInteger b)

b28Decode :: BS.ByteString
          -> Maybe BS.ByteString
b28Decode input = liftM (BS.append prefix) r
  where
    (z,b)  = BS.span (== b28 0) input
    prefix = BS.map (fromJust . b28') z -- preserve leading 1's
    r | BS.null b = Just BS.empty
      | otherwise = integerToBS <$> b28DecodeInt b

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
