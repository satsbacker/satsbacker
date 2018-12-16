{-# LANGUAGE RecordWildCards #-}

module Bitsbacker.Server where

import Database.SQLite.Simple (Connection)
import Lucid
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (MVar)
import Network.Wai (Middleware)
import System.Environment (lookupEnv)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Scotty
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Text.Mustache
import Data.Aeson

import Invoicing
import Bitsbacker.Templates
import Bitsbacker.Config
import Bitsbacker.Data.User
import Bitsbacker.Html

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

getTemplate :: Template -> TemplateType -> Template
getTemplate templates typ =
    templates { templateActual = templateName typ }

-- getTemplate :: [BBTemplate] -> TemplateType -> Template
-- getTemplate templates typ =
--   let
--       mtemplate = find ((==typ) . bbTemplateType) templates
--       err = T.unpack (unPName (templateName typ)) ++ " template not loaded"
--   in
--       maybe (error err) bbTemplate mtemplate

-- getUserPage :: User -> UserPage
-- getUserPage user = do


lookupUserPage :: MVar Connection -> Template -> ActionM ()
lookupUserPage mvconn templ = do
  username <- param "user"
  muser <- liftIO $ getUser mvconn (Username username)
  case muser of
    Nothing   -> next
    Just (userId, user) -> do
        stats <- liftIO $ getUserStats mvconn userId
        let userPage = UserPage user stats
            (_warnings, rendered) = renderMustacheW templ (toJSON userPage)
        html rendered


routes :: Config -> Template -> ScottyM ()
routes cfg@Config{..} templates = do
  get  "/"        (content home)
  get  "/signup"  (content signup)
  post "/signup"  postSignup
  get  "/:user"   (lookupUserPage cfgConn (t UserTemplate))
  invoiceRoutes cfg
  middleware (static "public")
  where
    t = getTemplate templates


static :: String -> Network.Wai.Middleware
static path =
  staticPolicy (addBase path)

printTemplateErrors :: Show a => a -> IO ()
printTemplateErrors errors = print errors

server :: IO ()
server = do
  cfg <- getConfig
  startServer cfg

startServer :: Config -> IO ()
startServer cfg = do
  port      <- getPort
  templates <- loadTemplates
  -- TODO loaded template stats (# loaded, etc)
  scotty port (routes cfg templates)


getPort :: IO Int
getPort = do
  mstrport <- lookupEnv "PORT"
  return (fromMaybe 8002 (mstrport >>= readMaybe))
