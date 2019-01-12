{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Data.TiersPage
    ( TiersPage(..)
    , mkTiersPage
    , tiersPageToJSON
    ) where

import Data.Aeson
import Data.Text (Text)

import Satsbacker.AmountConfig

import Satsbacker.Data.User
import Satsbacker.Data.Tiers


data TiersPage = TiersPage {
      tiersPageUser   :: User
    , tiersRows       :: [TierCols]
    , tiersColumnWidth :: Text
    , tiersNumColumns  :: Int
    }


tiersPageToJSON :: AmountConfig -> TiersPage -> Value
tiersPageToJSON acfg TiersPage{..} =
  object [ "tiers"    .= map (tierColsToJSON acfg) tiersRows
         , "user"     .= tiersPageUser
         , "colwidth" .= tiersColumnWidth
         , "ncolumns" .= tiersNumColumns
         ]

mkTiersPage :: User -> Int -> [Tier] -> TiersPage
mkTiersPage user cols tiers =
  TiersPage {
    tiersPageUser   = user
  , tiersRows       = map TierCols rs
  , tiersNumColumns = cols
  , tiersColumnWidth =
      case cols of
        2 -> "one-half"
        3 -> "one-third"
        4 -> "one-fourth"
        _ -> ""
  }
  where
    rs :: [[Tier]]
    rs = foldr folder [] tiers
    folder tier rows =
        case rows of
          [] -> [[tier]]
          row:rest
            | length row == cols -> [tier] : row : rest
            | otherwise          -> (tier:row) : rest


