{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Satsbacker.Email where

import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Mail.SMTP
import Satsbacker.Config (Config(..))
import Satsbacker.Data.Email (Email(..))

import Satsbacker.Templates.Render (renderTemplate)
import Satsbacker.Data.Site
import Satsbacker.Data.User (User)

import Crypto.Macaroons
import qualified Crypto.Macaroons as Macaroon

confirmLink :: Protocol -> HostName -> Macaroon -> Text
confirmLink (Protocol proto) (HostName host) macaroon =
    let
        bakedMacaroon = decodeUtf8 (Macaroon.serialize macaroon)
    in
        proto <> "://" <> host <> "/confirm-email/" <> bakedMacaroon

signupEmail :: Config -> User -> Macaroon -> Email -> IO ()
signupEmail cfg@Config{..} user macaroon (Email toAddress) =
  renderSendMail mail
  where
    mail    = simpleMail cfgEmail to cc bcc subject [body, html]
    cc      = []
    bcc     = []
    to      = [ Address Nothing toAddress ]
    subject = "Confirm your email address"

    hostname = siteHostName cfgSite
    clink = confirmLink proto hostname macaroon

    proto = siteProtocol cfgSite

    dat = object [ "user" .= user
                 , "confirmation-link" .= clink
                 ]

    body = plainTextPart (renderTemplate cfg "confirm-email-txt" dat)
    html = htmlPart (renderTemplate cfg "confirm-email" dat)


