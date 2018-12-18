{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import System.Environment (getArgs)
import Database.SQLite.Simple (Connection)
import Control.Concurrent.MVar (withMVar)

import Bitcoin.Denomination

import Bitsbacker.Cli
import Bitsbacker.Config
import Bitsbacker.Data.User
import Bitsbacker.Data.Tiers
import Bitsbacker.DB.Table (insertL, insert)

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
      newT = newTier userId
  in
  newT {
    tierDescription = "Don't have much? That's ok, everything helps!"
  , tierAmountFiat = 1
  , tierAmountMSats = toMsats (bits 100)
  } :
  newT {
    tierDescription = "Gain access to periodic backer-only blog posts and updates"
  , tierAmountFiat = 5
  , tierAmountMSats = toMsats (bits 1000)
  } :
  newT {
    tierDescription = T.unwords $
      "In addition to the previous levels, " :
      "gain access to the bitsbacker beta " :
      "when it becomes available." : []
  , tierAmountFiat = 15
  , tierAmountMSats = toMsats (bits 5000)
  } :
  newT {
    tierDescription = T.unwords $
      "In addition to the previous levels, recieve a premium " :
      "bitsbacker account, where your account will be " :
      "optionally listed in the bitsbacker hall of fame. " : []
  , tierAmountFiat = 50
  , tierAmountMSats = toMsats (bits 10000)
  } :
  []


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
