
module Satsbacker.Data.Merged
    ( Merged(..)
    ) where

import Data.Aeson
import Data.Text (Text)

import qualified Data.HashMap.Lazy as Map


newtype Merged a into = Merged { getMergedConfig :: ((Text, a), into) }

instance (ToJSON a, ToJSON into) => ToJSON (Merged a into) where
    toJSON (Merged ((key, obj), intoVal)) =
        let
            cfgObj v intoV = Object (Map.insert key v intoV)
        in
        case (toJSON intoVal, toJSON obj) of
          (Object intoV, valJson) -> cfgObj valJson intoV
          (_, valJson)            -> cfgObj valJson mempty -- don't merge if we see a non-object
