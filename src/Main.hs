{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import System.Environment (getArgs)
import Control.Concurrent.MVar (withMVar)

import Bitcoin.Denomination

import Bitsbacker.Cli
import Bitsbacker.Config
import Bitsbacker.Data.User
import Bitsbacker.Data.Tiers
import Bitsbacker.DB.Table (insert)

import qualified Data.Text as T


main :: IO ()
main = do
  args <- fmap (map T.pack) getArgs
  mainWith args


mainWith :: [Text] -> IO ()
mainWith args = do
  cfg <- getConfig

  case args of
    (x:xs) -> processArgs cfg x xs
    []     -> usage


testTierData :: UserId -> [TierDef]
testTierData userId =
  let
      newT f b d =
        (newTier userId) {
          tierDescription = d
        , tierAmountFiat = f
        , tierAmountMSats = (toMsats b)
        }
  in
  [
    newT 1 (bits 100) "Don't have much? That's ok, everything helps!"
  , newT 5 (bits 1000) "Gain access to periodic backer-only blog posts and updates"
  , newT 15 (bits 5000) $ T.unwords $
      "In addition to the previous levels, " :
      "gain access to the bitsbacker beta " :
      "when it becomes available." : []
  , newT 50 (bits 10000) $ T.unwords $
      "In addition to the previous levels, recieve a premium " :
      "bitsbacker account, where your account will be " :
      "optionally listed in the bitsbacker hall of fame. " : []
  ]

testTiers :: UserId -> [Tier]
testTiers userId =
    flip map (testTierData userId) $ \tdef ->
        Tier { tierDef = tdef
             , tierId = 1
             , tierStats = TierStats 0
             }

testData :: IO ()
testData = do
  Config{..} <- getConfig
  user' <- createUser (Plaintext "test")
  let user = user' { userEmail = Email "jb55@jb55.com"
                   , userName  = Username "jb55"
                   , userMaking = "bitsbacker"
                   }
  withMVar cfgConn $ \conn -> do
    userId <- insert conn user
    let tiers = testTierData (UserId userId)
    mapM_ (insert conn) tiers
  return ()
