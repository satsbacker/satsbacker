
module Bitsbacker.Cli where

import Database.SQLite.Simple (Connection)
import System.Exit (exitFailure)
import Data.Text (Text)

import qualified Data.Text as T

import Bitsbacker.Data.User
import Bitsbacker.Server

createUserUsage :: IO ()
createUserUsage = do
  putStrLn "usage: bitsbacker create-user <name> <email> <password> [is-admin]"
  exitFailure


createUserCmd :: Connection -> [Text] -> IO ()
createUserCmd conn args = do
  case args of
    (name:email:pass:adminArg) -> do
      user_ <- createUser (Plaintext pass)
      let isAdmin = not (null adminArg)
          user = user_ {
                    userName        = Username name
                  , userEmail       = Email email
                  , userPermissions = Permissions (if isAdmin then 1 else 0)
                  }
      userId <- insertUser conn user
      putStrLn ("created " ++ (if isAdmin then "admin" else "normal")
                           ++ " user "
                           ++ ('\'' : T.unpack name) ++ "'"
                           ++ " <" ++ T.unpack email ++ ">"
                           ++ " (id:" ++ show userId ++ ")"
                           )
    _ -> createUserUsage


processArgs :: Connection -> Text -> [Text] -> IO ()
processArgs conn arg rest =
    case (arg, rest) of
      ("create-user", args) ->
          createUserCmd conn args

      ("server", _) ->
          startServer conn

      (_, _) ->
          usage


usage :: IO ()
usage = do
  putStrLn "usage: bitsbacker <command>"
  putStrLn ""
  putStrLn "commands:"
  putStrLn ""
  putStrLn "  - create-user"
  putStrLn ""
  exitFailure


