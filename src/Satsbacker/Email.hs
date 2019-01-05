{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Satsbacker.Email where

import Satsbacker.Config (Config(..))
import Satsbacker.Data.Email (Email(..))
import Network.Mail.SMTP
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Satsbacker.Templates.Render (renderTemplate)
import Satsbacker.Data.Site (Site(..))
import Satsbacker.Data.User (User)

import Crypto.Macaroons
import qualified Crypto.Macaroons as Macaroon


confirmLink :: Site -> Macaroon -> Text
confirmLink Site{..} macaroon =
    let
        bakedMacaroon = decodeUtf8 (Macaroon.serialize macaroon)
    in
        "https://" <> siteName <> "/confirm-email/" <> bakedMacaroon


signupEmail :: Config -> User -> Macaroon -> Email -> IO ()
signupEmail cfg@Config{..} user macaroon (Email toAddress) =
  renderSendMail mail
  where
    mail    = simpleMail cfgEmail to cc bcc subject [body, html]
    cc      = []
    bcc     = []
    to      = [ Address Nothing toAddress ]
    subject = "Confirm your email address"

    clink = confirmLink cfgSite macaroon

    dat = object [ "user" .= user, "confirmation-link" .= clink ]

    body = plainTextPart (renderTemplate cfg "confirm-email-txt" dat)
    html = htmlPart (renderTemplate cfg "confirm-email" dat)


