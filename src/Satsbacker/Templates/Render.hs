{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Templates.Render where

import Data.Aeson
import Text.Mustache
import Web.Scotty
import Data.Text (Text)

import Satsbacker.Data.Merged
import Satsbacker.Config

import qualified Data.Text.Lazy as LT

renderTemplate :: ToJSON a => Config -> Text -> a -> LT.Text
renderTemplate cfg t v = renderTemplateV cfg t (toJSON v)

renderTemplateV :: Config -> Text -> Value -> LT.Text
renderTemplateV cfg@Config{..} templ val =
    let template = cfgTemplates { templateActual = PName templ }
        templateData = Merged (("config", cfg), val)
        templateJson = toJSON templateData
        (_w, rendered) = renderMustacheW template templateJson
    in rendered


renderTemplateM :: ToJSON a => Config -> Text -> a -> ActionM ()
renderTemplateM c t = renderTemplateMV c t . toJSON


renderTemplateMV :: Config -> Text -> Value -> ActionM ()
renderTemplateMV c t = html . renderTemplateV c t
