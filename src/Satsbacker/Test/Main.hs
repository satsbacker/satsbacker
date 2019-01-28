{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad.Logger
import System.Directory (removeFile)
import Test.Hspec

import Satsbacker.Config
import Satsbacker.DB

import Satsbacker.Test.Subscriptions
import Satsbacker.Test.Config

type Runner m = forall a. m a -> IO a

main :: IO ()
main = mainNoLog

cleanupDb :: Config -> IO ()
cleanupDb Config{..} =
  let network  = lncfgNetwork cfgLnConfig
      filename = dbFilename network
  in removeFile filename

mainRun :: (MonadIO m, MonadLogger m) => Runner m -> IO ()
mainRun runner =
  bracket (runner testConfig) cleanupDb (allTests runner)

mainNoLog :: IO ()
mainNoLog = mainRun runNoLoggingT

mainWithLog :: IO ()
mainWithLog = mainRun runStderrLoggingT

allTests :: (MonadIO m, MonadLogger m) => Runner m -> Config -> IO ()
allTests runner cfg = hspec $ do
  runIO (subscriptionTests runner cfg)

