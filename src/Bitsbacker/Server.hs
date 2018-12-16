{-# LANGUAGE RecordWildCards #-}

module Bitsbacker.Server where

import Database.SQLite.Simple (Connection)
import Lucid
import Control.Monad.IO.Class (liftIO)
import Network.RPC.Config (SocketConfig(..))
import Network.Wai (Middleware)
import System.Environment (lookupEnv)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Scotty
import Data.List
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Text.Mustache
import Data.Aeson
import qualified Data.Text as T

import Invoicing
import Bitsbacker.Templates
import Bitsbacker.DB
import Bitsbacker.Config
import Bitsbacker.Data.User (getUser, Username(..))
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


getTemplate :: [BBTemplate] -> TemplateType -> Template
getTemplate templates typ =
  let
      mtemplate = find ((==typ) . bbTemplateType) templates
      err = T.unpack (unPName (templateName typ)) ++ " template not loaded"
  in
      maybe (error err) bbTemplate mtemplate

-- getUserPage :: User -> UserPage
-- getUserPage user = do


lookupUserPage :: Connection -> Template -> ActionM ()
lookupUserPage conn templ = do
  username <- param "user"
  muser <- liftIO $ getUser conn (Username username)
  case muser of
    Nothing   -> next
    Just user ->
        let (_warnings, rendered) = renderMustacheW templ (toJSON user)
        in html rendered


routes :: Config -> [BBTemplate] -> ScottyM ()
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
  case templates of
    Left errors     -> printTemplateErrors errors
    Right templates_ ->
      scotty port (routes cfg templates_)


getPort :: IO Int
getPort = do
  mstrport <- lookupEnv "PORT"
  return (fromMaybe 8002 (mstrport >>= readMaybe))
