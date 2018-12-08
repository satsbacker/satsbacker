{-# LANGUAGE ScopedTypeVariables #-}

module Network.RPC.Error
    ( readError
    , catching
    , connectionError
    , jsonDecodeError
    , rpcErr
    , rpcErrStr
    , timeout
    , timeoutError
    , writeError
    , RPCError
    , RPCErrorType
    ) where

import Data.Maybe (fromMaybe)
import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified System.Timeout as Sys


data RPCErrorType = ConnectionError
                  | ReadError
                  | WriteError
                  | Timeout
                  | JsonDecodeError
                  deriving Show

data RPCError = RPCError RPCErrorType (Maybe ByteString)
              deriving Show

rpcErr :: RPCErrorType -> Maybe ByteString -> RPCError
rpcErr err m = RPCError err m

rpcErrStr :: RPCErrorType -> ByteString -> RPCError
rpcErrStr err s = RPCError err (Just s)

timeoutError :: RPCError
timeoutError = rpcErr Timeout Nothing

connectionError :: ByteString -> RPCError
connectionError = rpcErrStr ConnectionError

jsonDecodeError :: ByteString -> RPCError
jsonDecodeError = rpcErrStr JsonDecodeError

readError :: ByteString -> RPCError
readError = rpcErrStr ReadError

writeError :: ByteString -> RPCError
writeError = rpcErrStr WriteError

timeout :: Int -> IO (Either RPCError a) -> IO (Either RPCError a)
timeout tout io = do
  mres <- Sys.timeout tout io
  return (fromMaybe (Left timeoutError) mres)

catching :: (ByteString -> a) -> IO b -> IO (Either a b)
catching err io = catch newIO handler
  where
    newIO = do
      res <- io
      return (Right res)

    handler (e :: SomeException) =
      return (Left $ err (B8.pack (show e)))
