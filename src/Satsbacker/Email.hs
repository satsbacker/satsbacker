{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Satsbacker.Email where

import Satsbacker.Config (Config(..))
import Satsbacker.Data.Email (Email(..))
import Network.Mail.SMTP
import Data.Aeson (ToJSON)
import Text.Mustache (Template)

import Satsbacker.Templates (getTemplate, renderTemplate)


signupEmail :: ToJSON a
            => Config -> a -> Email -> Template -> IO ()
signupEmail cfg@Config{..} user (Email toAddress) templates =
  renderSendMail mail
  where
    mail    = simpleMail cfgEmail to cc bcc subject [body, html]
    cc      = []
    bcc     = []
    to      = [ Address Nothing toAddress ]
    subject = "Confirm your email address"

    bodyTemplate = getTemplate templates "confirm-email-txt"
    htmlTemplate = getTemplate templates "confirm-email"

    body = plainTextPart (renderTemplate cfg bodyTemplate user)
    html = htmlPart (renderTemplate cfg htmlTemplate user)


