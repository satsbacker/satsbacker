
module Bitsbacker.DB.SQLite
    ( migrate
    ) where

newtype DBVersion = DBVersion { getDBVersion :: Int }
