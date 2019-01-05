{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Cli where

import Database.SQLite.Simple (Connection)
import Control.Concurrent.MVar (MVar, withMVar)
import System.Exit (exitFailure)
import Data.Text (Text)

import qualified Data.Text as T

import Bitcoin.Denomination

import Satsbacker.Config
import Database.SQLite.Table (insert)
import Satsbacker.Data.Email
import Satsbacker.Data.Tiers
import Satsbacker.Data.User
import Satsbacker.Server

createUserUsage :: IO ()
createUserUsage = do
  putStrLn "usage: satsbacker create-user <name> <email> <password> [is-admin]"
  exitFailure


createUserCmd :: MVar Connection -> [Text] -> IO ()
createUserCmd mvconn args = do
  case args of
    (name:email:pass:adminArg) -> do
      user_ <- createUser (Plaintext pass)
      let isAdmin = not (null adminArg)
          user = user_ {
                    userName        = Username name
                  , userEmail       = Email email
                  , userPermissions = Permissions (if isAdmin then 1 else 0)
                  }
      userId <- withMVar mvconn $ \conn -> insert conn user
      putStrLn ("created " ++ (if isAdmin then "admin" else "normal")
                           ++ " user "
                           ++ ('\'' : T.unpack name) ++ "'"
                           ++ " <" ++ T.unpack email ++ ">"
                           ++ " (id:" ++ show userId ++ ")"
                           )
    _ -> createUserUsage


processArgs :: Config -> Text -> [Text] -> IO ()
processArgs cfg@Config{..} arg rest =
    case (arg, rest) of
      ("create-user", args) ->
          createUserCmd cfgConn args

      ("server", _) ->
          startServer cfg

      ("test-data", _) ->
          testData

      (_, _) ->
          usage


usage :: IO ()
usage = do
  putStrLn "usage: satsbacker <command>"
  putStrLn ""
  putStrLn "commands:"
  putStrLn ""
  putStrLn "  create-user"
  putStrLn "  server"
  putStrLn "  test-data"
  putStrLn ""
  exitFailure




testData :: IO ()
testData = do
  Config{..} <- getConfig
  user' <- createUser (Plaintext "test")
  let user = user' { userEmail = Email "jb55@jb55.com"
                   , userName  = Username "jb55"
                   , userMaking = "satsbacker"
                   }
  withMVar cfgConn $ \conn -> do
    userId <- insert conn user
    let tiers = testTierData (UserId userId)
    mapM_ (insert conn) tiers
  return ()


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
    newT 1 (bits 100) "Don't have much? That's ok, every bit helps!"
  , newT 5 (bits 1000) "Gain access to periodic backer-only blog posts and updates"
  , newT 15 (bits 5000) $ T.unwords $
      "In addition to the previous levels, " :
      "gain access to the satsbacker beta " :
      "when it becomes available." : []
  , newT 50 (bits 10000) $ T.unwords $
      "In addition to the previous levels, recieve a premium " :
      "satsbacker account, where your account will be " :
      "optionally listed in the satsbacker hall of fame. " : []
  ]

testTiers :: UserId -> [Tier]
testTiers userId =
    flip map (testTierData userId) $ \tdef ->
        Tier { tierDef = tdef
             , tierId = 1
             , tierStats = TierStats 0
             }
