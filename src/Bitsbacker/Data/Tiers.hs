{-# LANGUAGE RecordWildCards #-}

module Bitsbacker.Data.Tiers where

import Bitsbacker.DB.Table
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Control.Concurrent.MVar
import Data.Aeson

import Bitcoin.Denomination

import Bitsbacker.Data.User (UserId(..))

import Data.Text (Text)

import qualified Data.Text as T

data TierType = Standard
              | Custom
              deriving (Enum, Show, Eq)

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

newtype TierCols = TierCols { getTierCols :: [Tier] }
    deriving Show

instance ToJSON TierType where
    toJSON Standard = toJSON ("standard" :: Text)
    toJSON Custom   = toJSON ("custom" :: Text)

instance ToJSON TierDef where
    toJSON TierDef{..} =
        object [ "description"  .= tierDescription
               , "quota"        .= tierQuota
               , "amount_fiat"  .= tierAmountFiat
               , "amount_msats" .= tierAmountMSats
               , "amount_bits"  .= showBits (toBits tierAmountMSats)
               , "type"         .= tierAmountMSats
               , "is_custom"    .= (tierType == Custom)
               ]

instance ToJSON TierStats where
    toJSON TierStats{..} =
        object ["subs" .= tierSubs]

instance ToJSON Tier where
    toJSON Tier{..} =
        case (toJSON tierDef, toJSON tierStats) of
          (Object d, Object s) -> Object (d <> s)
          _ -> Object mempty


instance ToJSON TierCols where
    toJSON (TierCols tiers) =
        object ["columns" .= tiers]



tierDefFields :: [Text]
tierDefFields =
    [ "description"
    , "quota"
    , "user_id"
    , "amount_fiat"
    , "amount_msats"
    , "type"
    ]


tierStatsFields :: [Text]
tierStatsFields =
    [ "subs"
    ]


tierFields :: [Text]
tierFields = tierDefFields <> tierStatsFields


instance Table TierDef where
    tableName _   = "tiers"
    tableFields _ = tierDefFields


instance ToField TierType where
    toField = toField . fromEnum


instance FromField TierType where
    fromField f = fmap toEnum (fromField f)


instance FromRow TierStats where
    fromRow = TierStats <$> field


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


getTiers :: MVar Connection -> UserId -> IO [Tier]
getTiers mvconn (UserId userId) = do
  let tdFields = T.intercalate ", " (map ("t."<>) tierDefFields)
      q = "SELECT "<>tdFields<>", count(s.tier_id) as subs "
        <>"FROM tiers t LEFT JOIN subscriptions s "
        <>" ON s.tier_id = t.id AND s.valid_until < CURRENT_TIMESTAMP "
        <>"WHERE t.user_id = ? "
        <>"GROUP BY " <> tdFields
  withMVar mvconn $ \conn -> do
    tdefs <- query conn (Query q) (Only userId)
    return $ flip map tdefs $ \(td@TierDef{} :. ts@TierStats{}) ->
      Tier {
        tierDef   = td
      , tierStats = ts
    }


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
