{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Lucid
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Environment
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import Web.Scotty


import Bitsbacker.Html
import Bitsbacker.DB (migrate, openDb)
import Bitsbacker.Data.User

import qualified Data.Text as T

getPort :: IO Int
getPort = do
  mstrport <- lookupEnv "PORT"
  return (fromMaybe 8002 (mstrport >>= readMaybe))

home :: Html ()
home = do
  template Nothing $ do
    h1_ "Hello, world!"

postSignup :: ActionM ()
postSignup = do
  name  <- param "name"
  email <- param "email"
  text (name <> email)

signup :: Html ()
signup = do
  template (Just "signup") $ do
    h1_ "Signup"
    form_ $ do
      textInput "name"
      textInput "email"

content :: Html a -> ActionM ()
content = html . renderText

static :: String -> Network.Wai.Middleware
static path =
  staticPolicy (addBase path)

routes :: ScottyM ()
routes = do
  get  "/"       (content home)
  get  "/signup" (content signup)
  post "/signup" postSignup
  middleware (static "public")

createUserUsage :: IO ()
createUserUsage = do
  putStrLn "usage: bitsbacker create-user <name> <email> <password> [is-admin]"
  exitFailure

usage :: IO ()
usage = do
  putStrLn "usage: bitsbacker <command>"
  exitFailure

startServer :: IO ()
startServer = do
  port <- getPort
  scotty port routes

createUserCmd :: Connection -> [Text] -> IO ()
createUserCmd conn args = do
  case args of
    (name:email:pass:adminArg) -> do
      user_ <- createUser (Plaintext pass)
      let isAdmin = not (null adminArg)
          user = user_ {
                    userName  = Username name
                  , userEmail = Email email
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
          startServer

      (_, _) ->
          usage

mainWith :: [Text] -> IO ()
mainWith args = do
  conn <- openDb
  migrate conn

  case args of
    (x:xs) -> processArgs conn x xs
    []     -> usage

main :: IO ()
main = do
  args <- fmap (map T.pack) getArgs
  mainWith args
