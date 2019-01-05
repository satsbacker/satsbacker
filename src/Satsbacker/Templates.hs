{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Templates where


import Data.Aeson
import Data.List (isSuffixOf)
import Text.Mustache
import Web.Scotty (html, ActionM)
import Data.Text (Text)

import qualified Data.HashMap.Lazy as Map
import qualified Data.Text.Lazy as LT

import Satsbacker.Config (Config(..))


newtype Merged a into = Merged { getMergedConfig :: ((Text, a), into) }

instance (ToJSON a, ToJSON into) => ToJSON (Merged a into) where
    toJSON (Merged ((key, obj), intoVal)) =
        let
            cfgObj v intoV = Object (Map.insert key v intoV)
        in
        case (toJSON intoVal, toJSON obj) of
          (Object intoV, valJson) -> cfgObj valJson intoV
          (_, valJson)            -> cfgObj valJson mempty -- don't merge if we see a non-object


loadTemplates :: IO Template
loadTemplates =
  compileMustacheDir' (\s -> "html" `isSuffixOf` s
                          || ".txt" `isSuffixOf` s)
                      (PName "user")
                      "templates"



getTemplate :: Template -> Text -> Template
getTemplate templates pname =
    templates { templateActual = PName pname }



renderTemplate :: ToJSON a => Config -> Template -> a -> LT.Text
renderTemplate cfg templ val =
    let templateData = Merged (("config", cfg), val)
        templateJson = toJSON templateData
        (_w, rendered) = renderMustacheW templ templateJson
    in rendered


renderTemplateM :: ToJSON a => Config -> Template -> a -> ActionM ()
renderTemplateM c t = html . renderTemplate c t

