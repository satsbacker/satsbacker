{-# LANGUAGE RecordWildCards #-}

module Bitsbacker.Data.Tiers where

import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Aeson

import Bitcoin.Denomination
import Bitsbacker.DB.Table
import Bitsbacker.Data.User (UserId(..))

import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

data TierType = Standard
              | Custom
              deriving (Enum, Show, Eq)

data TierDef = TierDef {
      tierDescription :: Text
    , tierQuota       :: Maybe Int
    , tierUserId      :: UserId
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
      tierId   :: Int
    , tierDef   :: TierDef
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
          (Object d, Object s) ->
              let obj = d <> s
                  tid = M.insert "tier_id" (toJSON tierId) obj
              in Object tid
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

newtype TierId = TierId { getTierId :: Int }
    deriving (Show, Eq, Ord)

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
            , getUserId tierUserId
            , tierAmountFiat
            , getMsats tierAmountMSats
            , tierType
            )


instance FromRow TierDef where
  fromRow = TierDef <$> field
                    <*> field
                    <*> fmap UserId field
                    <*> field
                    <*> fmap MSats field
                    <*> field

newtype Limit = Limit { getLimit :: Int }
    deriving Show

noLimit :: Limit
noLimit = Limit 0

newtype Search a = Search { getSearch :: (Text, a, Limit) }
    deriving Show

search :: Text -> a -> Limit -> Search a
search t v l = Search (t,v,l)

getTierById :: Connection -> TierId -> IO (Maybe Tier)
getTierById conn (TierId tierId) =
    listToMaybe <$> getTiers_ conn (search "id" tierId (Limit 1))

getTiers :: Connection -> UserId -> IO [Tier]
getTiers conn (UserId userId) =
    getTiers_ conn (search "user_id" userId noLimit)

getTiers_ :: ToField q => Connection -> Search q -> IO [Tier]
getTiers_ conn (Search (term, val, Limit lim)) = do
  let tdFields = T.intercalate ", " (map ("t."<>) tierDefFields)
      q = "SELECT t.id, "<>tdFields<>", count(s.tier_id) as subs "
        <>"FROM tiers t LEFT JOIN subscriptions s "
        <>" ON s.tier_id = t.id AND s.valid_until < CURRENT_TIMESTAMP "
        <>"WHERE t."<>term<>" = ? "
        <>"GROUP BY " <> tdFields
        <>(if lim == 0 then "" else " LIMIT " <> T.pack (show lim))
  tdefs <- query conn (Query q) (Only val)
  return $ flip map tdefs $ \(Only tid :. td@TierDef{} :. ts@TierStats{}) ->
    Tier {
      tierDef   = td
    , tierId    = tid
    , tierStats = ts
  }


newTier :: UserId -> TierDef
newTier uid@UserId{..} =
    TierDef {
      tierDescription = ""
    , tierQuota       = Nothing
    , tierUserId      = uid
    , tierAmountFiat  = 0
    , tierAmountMSats = msats 0
    , tierType        = Standard
    }
