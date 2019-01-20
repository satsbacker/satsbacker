{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Satsbacker.Test.Subscriptions where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Functor (void)
import Test.Hspec

import Database.SQLite.Simple
import Database.SQLite.Table

import Satsbacker.Data.Invoice
import Satsbacker.Config
import Satsbacker.Data.Tiers
import Satsbacker.Data.InvoiceId
import Satsbacker.Data.Email
import Satsbacker.Cli

import qualified Data.Text as T


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
  liftIO (testData mvconn)
  let testTierId = TierId 1
  InvoiceRef{..} <- liftIO (createTestInvRef testTierId mvconn)
  msub <- persistSubFromInvId mvconn invRefInvoiceId
  maybe (fail "expected to insert subscription") (void . return) msub



subscriptionTests :: (MonadIO m, MonadLogger m)
                  => (m () -> IO ()) -> Config -> IO ()
subscriptionTests runner Config{..} = hspec $ do
  describe "subscriptions" $ do
    it "create test subscription" (runner $ createTestSubscription cfgConn)
