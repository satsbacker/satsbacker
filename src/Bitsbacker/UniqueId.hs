{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Bitsbacker.UniqueId where

import Data.Int (Int64)
import Data.Bits ((.|.), shiftL)
import Data.Foldable (foldl')
import System.Entropy (getEntropy)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BUIDL
import Data.ByteString (ByteString)

import Bitsbacker.Base28 (b28Encode)

newtype UniqueId = UniqueId { getUniqueId :: Int64 }

instance Show UniqueId where
  show invoiceId = B8.unpack (encodeUniqueId invoiceId)

encodeUniqueId :: UniqueId -> ByteString
encodeUniqueId (UniqueId uuid) =
  b28Encode uuidBytes
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

newUniqueId :: IO UniqueId
newUniqueId = fmap UniqueId randInt

-- λ> replicateM 5 newUniqueId >>= mapM_ print
-- C5YYNAS2UPGPR
-- N7XMRMMMQY3SM
-- CNUWBCP3QZ7DU
-- KLP5QWNBMR2P6
-- DQWK3M2XMHZLV

-- decodeUniqueId :: ByteString -> Maybe UUID
-- decodeUniqueId bs =
--   fmap LBS.fromStrict (b32Decode bs) >>= UUID.fromByteString
