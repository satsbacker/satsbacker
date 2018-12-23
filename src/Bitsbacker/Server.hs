{-# LANGUAGE RecordWildCards #-}

module Bitsbacker.Server where

import Control.Concurrent (MVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple (Connection)
import Lucid
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Environment (lookupEnv)
import Text.Mustache
import Text.Read (readMaybe)
import Web.Scotty

import Invoicing

import Bitsbacker.Templates
import Bitsbacker.Logging
import Bitsbacker.Config
import Bitsbacker.Data.User
import Bitsbacker.Data.Tiers
import Bitsbacker.Data.TiersPage
import Bitsbacker.Data.Checkout
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


getTemplate :: Template -> PName -> Template
getTemplate templates pname =
    templates { templateActual = pname }


lookupUserPage :: MVar Connection -> Template -> ActionM ()
lookupUserPage mvconn templ = do
  (userId, user) <- withUser mvconn
  stats <- liftIO $ getUserStats mvconn userId
  let userPage = UserPage user stats
      (_warnings, rendered) = renderMustacheW templ (toJSON userPage)
  html rendered


simplePage :: ToJSON a => Template -> a -> ActionM ()
simplePage templ val = html page
  where
    (_warnings, page) = renderMustacheW templ (toJSON val)



withUser :: MVar Connection -> ActionM (UserId, User)
withUser mvconn = do
  username <- param "user"
  muser <- liftIO $ withMVar mvconn $ \conn -> getUser conn (Username username)
  case muser of
    Nothing -> next
    Just user -> return user


tiersPage :: MVar Connection -> Template -> ActionM ()
tiersPage mvconn templ = do
  (userId, user) <- withUser mvconn
  tiers <- liftIO $ withMVar mvconn $ \conn -> getTiers conn userId
  let tpage = mkTiersPage user 2 tiers
      (_warnings, rendered) = renderMustacheW templ (toJSON tpage)
  html rendered


checkoutErrorPage :: CheckoutError -> ActionM ()
checkoutErrorPage = raise . describeCheckoutError


checkout :: Config -> Template -> ActionM ()
checkout cfg@Config{..} templ = do
  itierId <- param "tier_id"
  let tierId = TierId itierId
  echeckoutPage <- liftIO (mkCheckoutPage cfg tierId)
  case echeckoutPage of
    Left err -> do liftIO $ logError (show err)
                   checkoutErrorPage err
    Right checkoutPage -> do
      let checkoutJson = toJSON checkoutPage
          (_w, rendered) = renderMustacheW templ checkoutJson
      html rendered

routes :: Config -> Template -> ScottyM ()
routes cfg@Config{..} templates = do
  get  "/"                  (simplePage (t "home") ())
  get  "/signup"            (content signup)
  post "/signup"            postSignup
  get  "/:user"             (lookupUserPage cfgConn (t "user"))
  get  "/back/:user"        (tiersPage cfgConn (t "back"))
  get  "/checkout/:tier_id" (checkout cfg (t "checkout"))
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
