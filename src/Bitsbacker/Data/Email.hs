{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitsbacker.Data.Email
    ( Email(..)
    ) where

import Data.Text (Text)
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

newtype Email = Email { getEmail :: Text }
    deriving (Show, Eq, Ord, ToField, FromField)
