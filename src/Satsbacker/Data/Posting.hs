{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Data.Posting where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Table

import Satsbacker.Data.InvoiceId (InvId(..))
import Satsbacker.Data.Email (Email(..))

import Text.Read (readMaybe)
import Data.Text (Text)

import Bitcoin.Denomination (MSats(..))

data PostingNote = SubExpired
                 | SubNew
                 deriving (Enum, Show, Read, Eq, Ord)

data Posting = Posting {
      postingSubId     :: Int
    , postingUserEmail :: Email
    , postingAmount    :: MSats
    , postingNote      :: PostingNote
    , postingInvId     :: InvId
    }

instance Table Posting where
    tableName _   = "postings"
    tableFields _ = postingFields

instance ToField PostingNote where
    toField = toField . show

instance FromField PostingNote where
    fromField f = do t <- fromField f
                     maybe (fail []) return (readMaybe t)

instance FromRow Posting where
    fromRow =
        Posting <$> field
                <*> field
                <*> fmap MSats field
                <*> field
                <*> fmap InvId field

postingFields :: [Text]
postingFields =
  let
      Posting _sub_id
              _user_email
              _amount
              _note
              _invoice_id = undefined :: Posting
  in
    [ "sub_id"
    , "user_email"
    , "amount"
    , "note"
    , "invoice_id"
    ]


instance ToRow Posting where
    toRow posting =
        let Posting sub_id
                    user_email
                    amount
                    note
                    invoice_id = posting
        in
          toRow ( sub_id
                , user_email
                , getMsats amount
                , note
                , getInvId invoice_id
                )
