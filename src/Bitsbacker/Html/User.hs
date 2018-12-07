{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bitsbacker.Html.User where

import Bitsbacker.Data.User
import Bitsbacker.Html

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Lucid
import Web.Scotty

userPhrase :: Username -> Text -> Text
userPhrase (Username user) making =
    user <> " is making " <> making

userPage :: User -> Html ()
userPage User{..} =
  let
    phrase = toHtml (userPhrase userName userMaking)
  in
    template (Just phrase) $ do
      div_ [ class_ "hero" ] $ do
        h1_ phrase

lookupUserPage :: Connection -> ActionM ()
lookupUserPage conn = do
  username <- param "user"
  muser <- liftIO $ getUser conn (Username username)
  case muser of
    Nothing   -> next
    Just user -> content (userPage user)
