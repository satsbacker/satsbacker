{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Network.RPC.Common
    ( Resp
    , defaultTimeout
    , ShortId(..)
    ) where

import Data.Aeson
import Data.String (IsString)
import Data.Text (Text)

defaultTimeout :: Int
defaultTimeout = 5*1000000

newtype ShortId = ShortId { getShortId :: Text }
  deriving (IsString, FromJSON, ToJSON, Ord, Eq)

instance Show ShortId where
    show (ShortId t) = show t

type family Resp a :: *
