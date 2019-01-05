{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Cookies where

import Data.ByteString.Builder (toLazyByteString)
import Web.Cookie
import Web.Scotty

import Crypto.Macaroons

import qualified Crypto.Macaroons as Macaroon
import qualified Data.Text.Lazy.Encoding as TL

authCookie :: Macaroon -> SetCookie
authCookie macaroon =
  defaultSetCookie {
    setCookieName  = "session"
  , setCookieValue = Macaroon.serialize macaroon
  }



setCookie :: SetCookie -> ActionM ()
setCookie c =
  addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie c)
