{-# LANGUAGE RecordWildCards #-}

module Bitsbacker.Data.Tiers where

import Bitsbacker.DB.Table
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Bitcoin.Denomination (msats, MSats(..))

import Bitsbacker.Data.User (UserId(..))

import Data.Text (Text)

import qualified Data.Text as T

data TierType = Standard
              | Custom
              deriving (Enum, Show)

data TierDef = TierDef {
      tierDescription :: Text
    , tierQuota       :: Maybe Int
    , tierUserId      :: Int
    , tierAmountFiat  :: Int
    , tierAmountMSats :: MSats
    , tierType        :: TierType
    }
    deriving Show

data TierStats = TierStats {
      tierSubs :: Int
    }
    deriving Show

data Tier = Tier {
      tierDef   :: TierDef
    , tierStats :: TierStats
    }
    deriving Show

newtype TiersCols = TierCols [Tier]
    deriving Show

data TiersPage = TiersPage {
      tiersRows :: [TiersCols]
    }
    deriving Show

tierQueryFields :: [Text]
tierQueryFields =
    [ "description"
    , "quota"
    , "user_id"
    , "amount_fiat"
    , "amount_msats"
    , "type"
    ]

instance Table TierDef where
    tableName _   = "tiers"
    tableFields _ = tierQueryFields

instance ToField TierType where
    toField = toField . fromEnum

instance FromField TierType where
    fromField f = fmap toEnum (fromField f)

instance ToRow TierDef where
  toRow TierDef{..} =
      toRow ( tierDescription
            , tierQuota
            , tierUserId
            , tierAmountFiat
            , getMsats tierAmountMSats
            , tierType
            )

instance FromRow TierDef where
  fromRow = TierDef <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> fmap MSats field
                    <*> field

newTier :: UserId -> TierDef
newTier UserId{..} =
    TierDef {
      tierDescription = ""
    , tierQuota = Nothing
    , tierUserId = getUserId
    , tierAmountFiat = 0
    , tierAmountMSats = msats 0
    , tierType = Standard
    }
