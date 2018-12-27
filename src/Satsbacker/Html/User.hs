{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Satsbacker.Html.User where

import Satsbacker.Data.User
import Satsbacker.Html


import Data.Text (Text)

import Lucid


divc :: Term [Attribute] result => Text -> result
divc cls = div_ [ class_ cls ]

userPhrase :: Username -> Text -> Html ()
userPhrase (Username user) making =
    p_ $ do
      b_ (toHtml user)
      toHtml (" is making " <> making)

txtUserPhrase :: Username -> Text -> Text
txtUserPhrase (Username user) making =
    user <> " is making " <> making


becomeBacker :: User -> Html ()
becomeBacker = undefined


overview :: Html ()
overview =
  divc "overview" $ do
    oelem "backers"        (b_ "1000" >> " backers")
    oelem "bits-per-month" (b_ "100k" >> " bits/month")
  where
    oelem c = div_ [classes_ [c, "overview-elem"]]


goals :: Html ()
goals = do
  h2_ "Goals"


about :: Text -> Html ()
about about_ = do
  h2_ "About"
  p_ (toHtml about_)


-- userPage :: UserPage -> Html ()
-- userPage UserPage{..} =
--   let User{..}      = userPageUser
--       UserStats{..} = userPageStats
--   in
--     template (Just txtPhrase) $ do
--       div_ [ class_ "hero" ] $ do
--         h1_ phrase
--         overview
--       div_ [ class_ "container" ] $ do
--         -- about
--         goals
--         ul_ (li_ "goal")
--   where
--     phrase    = userPhrase userName userMaking
--     txtPhrase = txtUserPhrase userName userMaking


