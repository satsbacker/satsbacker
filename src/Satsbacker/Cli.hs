{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Cli where

import Control.Concurrent.MVar (MVar, withMVar)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import System.Exit (exitFailure)

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Bitcoin.Denomination

import Satsbacker.Config
import Database.SQLite.Table (insert)
import Satsbacker.Data.Email
import Satsbacker.Data.Tiers
import Satsbacker.Data.User
import Satsbacker.Server
import Satsbacker.AmountConfig

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

set :: ToField v => Connection -> Text -> v -> IO ()
set conn key v = execute conn q (Only v)
  where
    q = Query ("update site set (" <> key <> ") = ?")

setSiteConfig :: MVar Connection -> Text -> Text -> IO ()
setSiteConfig mvconn key val = do
  case key of
    "hostname"     -> withMVar mvconn $ \conn -> set conn "hostname" val
    "denomination" -> withMVar mvconn $ \conn ->
                        case parseDenomination val of
                          Nothing -> putStrLn "invalid denomination"
                          Just d  -> set conn "amount_type" d
    _          -> putStrLn ("unknown config key " ++ T.unpack key)

processArgs :: Config -> Text -> [Text] -> IO ()
processArgs cfg@Config{..} arg rest =
    case (arg, rest) of
      ("create-user", args) ->
          createUserCmd cfgConn args

      ("server", _) ->
          startServer cfg

      ("set", [key, val]) ->
          setSiteConfig cfgConn key val

      ("test-data", _) ->
          void (testData cfgConn)

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
  putStrLn "  set <SITEVAR> satsbacker.com"
  putStrLn ""
  putStrLn "SITEVARs"
  putStrLn ""
  putStrLn "  hostname"
  putStrLn "  denomination {sats,bits,BTC}"
  putStrLn ""
  exitFailure




testData :: MVar Connection -> IO (NonEmpty TierDef)
testData mvconn = do
  user' <- createUser (Plaintext "test")
  let user = user' { userEmail = Email "jb55@jb55.com"
                   , userName  = Username "jb55"
                   , userMaking = "satsbacker"
                   }
  withMVar mvconn $ \conn -> do
    userId <- insert conn user
    let tiers = testTierData (UserId userId)
    execute_ conn "update site set (hostname) = ('satsbacker.com')"
    mapM_ (insert conn) tiers
    return tiers


testTierData :: UserId -> NonEmpty TierDef
testTierData userId =
  let
      newT f b d =
        (newTier userId) {
          tierDescription = d
        , tierAmountFiat = f
        , tierAmountMSats = (toMsats b)
        }
  in
  newT 1 (bits 100) "Don't have much? That's ok, every bit helps!" :|
  [ newT 5 (bits 1000) "Gain access to periodic backer-only blog posts and updates"
  , newT 15 (bits 5000) $ T.unwords $
      "In addition to the previous levels, " :
      "gain access to the satsbacker beta " :
      "when it becomes available." : []
  , newT 50 (bits 10000) $ T.unwords $
      "In addition to the previous levels, recieve a premium " :
      "satsbacker account, where your account will be " :
      "optionally listed in the satsbacker hall of fame. " : []
  ]

testTiers :: UserId -> NonEmpty Tier
testTiers userId =
    flip NE.map (testTierData userId) $ \tdef ->
        Tier { tierDef = tdef
             , tierId = 1
             , tierStats = TierStats 0
             }
