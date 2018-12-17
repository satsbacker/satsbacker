
module Bitsbacker.Data.Tiers where

import Data.Text (Text)

data Tier = Tier {
      tierDescription :: Text
    , tierQuota       :: Maybe Int
    , tierSubs        :: Int
    }
    deriving Show

newtype TiersCols = TierCols [Tier]
    deriving Show

data TiersPage = TiersPage {
      tiersRows :: [TiersCols]
    }
    deriving Show
