{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Entropy where

import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Word (Word64)
import System.Entropy (getEntropy)
import Data.Bits ((.|.), shiftL)

import qualified Data.ByteString as BS

randInt :: IO Word64
randInt = do
    bs <- getEntropy 8
    return (w8int64 bs)


w8int64 :: ByteString -> Word64
w8int64 bytes = foldl' decodeInt 0 (BS.zip "\0\1\2\3\4\5\6\7\8" bytes)
  where
    decodeInt !n (i, byte) =
        n .|. fromIntegral byte `shiftL` (fromIntegral i * 8)

