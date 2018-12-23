-- base32string package

module Bitsbacker.Base32 ( Base32String
                         , b32Encode
                         , b32Decode
                         ) where

import           Control.Applicative   (pure, (<$>))
import           Control.Monad         (liftM)

import           Data.Bits             (shiftL, shiftR, (.|.))
import           Data.Char             (chr, ord)
import           Data.List             (unfoldr)

import           Data.Maybe            (fromJust, fromMaybe, isJust,
                                        listToMaybe)

import           Data.String           (fromString)
import           Data.Word             (Word8)
import           Numeric               (readInt, showIntAtBase)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as BSL

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

-- | Represents a Base32 string. Guarantees that all characters it contains
--   are valid base32 characters.
data Base32String =
  Base32String BS.ByteString
  deriving ( Show, Eq, Ord )

table :: BS.ByteString
table = BS.pack
        $  [65..90]
        ++ [50..55]

isValidBase32 :: Word8 -> Bool
isValidBase32 c =
  c `BS.elem` table

b32 :: Word8 -> Word8
b32 i = BS.index table (fromIntegral i)

b32' :: Word8 -> Maybe Word8
b32' w = fromIntegral <$> BS.elemIndex w table

b32EncodeInt :: Integer
             -> BS.ByteString
b32EncodeInt i =
    fromString $ showIntAtBase (32 :: Integer) f (fromIntegral i) ""
  where
    f = chr . fromIntegral . b32 . fromIntegral

b32DecodeInt :: BS.ByteString
             -> Maybe Integer
b32DecodeInt s = case go of
    Just (r,[]) -> Just r
    _           -> Nothing
  where
    c = b32' . fromIntegral . ord
    p = isJust . c
    f = fromIntegral . fromJust . c
    go = listToMaybe $ readInt 32 p f (BS8.unpack s)

b32Encode :: BS.ByteString
          -> BS.ByteString
b32Encode input = BS.append l r
  where
    (z, b) = BS.span (== 0) input
    l = BS.map b32 z -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = b32EncodeInt (bsToInteger b)

b32Decode :: BS.ByteString
          -> Maybe BS.ByteString
b32Decode input = liftM (BS.append prefix) r
  where
    (z,b)  = BS.span (== b32 0) input
    prefix = BS.map (fromJust . b32') z -- preserve leading 1's
    r | BS.null b = Just BS.empty
      | otherwise = integerToBS <$> b32DecodeInt b

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
