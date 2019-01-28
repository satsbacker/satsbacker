{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Satsbacker.Test.Subscriptions where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Functor (void)
import Data.Bool (bool)
import Test.Hspec

import Database.SQLite.Simple
import Database.SQLite.Table

import Satsbacker.Cli
import Satsbacker.Config
import Satsbacker.Data.Email
import Satsbacker.Data.Invoice
import Satsbacker.Data.InvoiceId
import Satsbacker.Data.Subscription
import Satsbacker.Data.Tiers
import Satsbacker.Donation
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE


createTestInvRef :: TierId -> MVar Connection -> IO InvoiceRef
createTestInvRef tierId mvconn = do
  newInvId <- newInvoiceId
  let testInv = testRef (InvId (T.pack (show newInvId)))
  _ <- insertL mvconn testInv
  return testInv
  where
    testRef invId = InvoiceRef
      { invRefInvoiceId = invId
      , invRefTierId    = tierId
      , invRefEmail     = Email "test@example.com"
      , invRefPayerId   = Nothing
      }


createTestSubscription :: (MonadIO m, MonadLogger m) => MVar Connection -> m ()
createTestSubscription mvconn = do
  tiers <- liftIO (testData mvconn)
  let TierDef{..} = NE.head tiers
      testTierId  = TierId 1
  InvoiceRef{..} <- liftIO (createTestInvRef testTierId mvconn)
  msub <- persistSubFromInvId mvconn invRefInvoiceId tierAmountMSats
  maybe (fail "expected to insert subscription") (void . return) msub



hasPositiveBalance :: MVar Connection -> SubId -> IO ()
hasPositiveBalance mvconn (SubId subId) = do
  (tierId :: Int, email :: Text) <- justOne $ withMVar mvconn $ \conn ->
      query conn "select tier_id, user_email from subscriptions where id = ?\
                  \ limit 1" (Only subId)

  TierDef{..}  <- fetchOneL' mvconn (search "id" tierId) :: IO TierDef
  balance      <- withMVar mvconn $ \conn ->
                    getEmailDonationBalance conn (Email email)

  bool (fail "user's balance doesn't match") (return ())
       (balance == tierAmountMSats)



subscriptionTests :: (MonadIO m, MonadLogger m)
                  => (m () -> IO ()) -> Config -> IO ()
subscriptionTests runner Config{..} = hspec $ do
  describe "subscriptions" $ do
    it "creation works" (runner (createTestSubscription cfgConn))
    it "have positive balance" (hasPositiveBalance cfgConn (SubId 1))
