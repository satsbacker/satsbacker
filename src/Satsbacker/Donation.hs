{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Satsbacker.Donation where


import Bitcoin.Denomination
import Database.SQLite.Simple

import Satsbacker.Data.Subscription
import Satsbacker.Data.Tiers
import Satsbacker.Data.User
import Satsbacker.Data.InvoiceId
import Satsbacker.Data.Email
import Satsbacker.Data.Posting




getEmailDonationBalance :: Connection -> Email -> IO MSats
getEmailDonationBalance conn (Email email) = do
  amt <- query conn "select sum(amount) from postings where user_email = ?"
           (Only email)
  case amt of
    []                -> return 0
    [Only (Just rs)]  -> return (MSats rs)
    [Only Nothing]    -> return 0
    _         -> fail "More than one result in getEmailDonationBalance"



subExpiredPosting :: InvId -> Email -> SubId -> TierDef -> Posting
subExpiredPosting invId email SubId{..} TierDef{..} =
    Posting {
      postingSubId      = getSubId
    , postingUserEmail  = email
    , postingAmount     = -tierAmountMSats
    , postingNote       = SubExpired
    , postingInvId      = invId
    }


-- processDonation :: Donation -> Posting
-- processDonation donation =

-- processDonations :: [Donation] -> [Posting]


-- processDonations :: Config -> IO ()
-- processDonations Config{..} = do
--   let q = Query "select * from subscriptions"
--   withMVar cfgConn $ \conn -> query conn ""

  -- TODO: fetch all the active subscriptions
  -- TODO: add any to the donation balance if missing
