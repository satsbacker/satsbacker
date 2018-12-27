{-# LANGUAGE RecordWildCards #-}

module Satsbacker.Server where

import Control.Concurrent (MVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Lucid
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Environment (lookupEnv)
import Text.Mustache
import Text.Read (readMaybe)
import Web.Scotty
import System.Posix.Env (putEnv)

import Invoicing

import Satsbacker.Config
import Satsbacker.Data.Checkout
import Satsbacker.Data.Email
import Satsbacker.Data.Invoice
import Satsbacker.Data.Tiers
import Satsbacker.Data.TiersPage
import Satsbacker.Data.User
import Satsbacker.Html
import Satsbacker.Logging
import Satsbacker.Templates
import Satsbacker.DB.Table


import qualified Data.HashMap.Lazy as Map
import qualified Data.Text.Lazy as LT


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


lookupUserPage :: Config -> Template -> ActionM ()
lookupUserPage cfg@Config{..} templ = do
  (userId, user) <- withUser cfgConn
  stats <- liftIO $ getUserStats cfgConn userId
  let userPage = UserPage user stats
  renderTemplate cfg templ userPage


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


tiersPage :: Config -> Template -> ActionM ()
tiersPage cfg@Config{..} templ = do
  (userId, user) <- withUser cfgConn
  tiers <- liftIO $ withMVar cfgConn $ \conn -> getTiers conn userId
  let tpage = mkTiersPage user 2 tiers
  renderTemplate cfg templ tpage


checkoutErrorPage :: CheckoutError -> ActionM ()
checkoutErrorPage = raise . describeCheckoutError

newtype MergedConfig a = MergedConfig { getMergedConfig :: (Config, a) }

instance ToJSON a => ToJSON (MergedConfig a) where
    toJSON (MergedConfig (cfg, val)) =
        case (toJSON val, toJSON cfg) of
          (Object valObj, cfgJson) ->
            Object (Map.insert "config" cfgJson valObj)
          (valJson, _) -> valJson -- don't merge if we see a non-object

renderTemplate :: ToJSON a => Config -> Template -> a -> ActionM ()
renderTemplate cfg templ val =
    let templateData = MergedConfig (cfg, val)
        templateJson = toJSON templateData
        (_w, rendered) = renderMustacheW templ templateJson
    in html rendered


postCheckout :: Config -> ActionM ()
postCheckout cfg@Config{..} = do
  tierId <- fmap TierId   (param "tier_id")
  user   <- fmap Username (param "user")
  email  <- fmap Email    (param "email")
  einv <- liftIO $ mkCheckout cfg tierId
  case einv of
    Left err ->
        let desc = describeCheckoutError err
            uri  = "/back/" <> getUsername user <> "?err=" <> desc
        in redirect (LT.fromStrict uri)
    Right inv -> do let invId  = invoiceId inv
                        invRef = InvoiceRef invId tierId email
                        coUri  = "/checkout/" <> invoiceId inv
                    _ <- liftIO $ withMVar cfgConn $ \conn ->
                           insert conn invRef
                    redirect (LT.fromStrict coUri)


getCheckout :: Config -> Template -> ActionM ()
getCheckout cfg@Config{..} templ = do
  invId   <- param "invoice_id"
  minvRef <- liftIO $ withMVar cfgConn $ \conn ->
               fetchOne conn (search "invoiceId" (invId :: Text))
  invRef  <- maybe (fail "checkout: could not find invoice id") return minvRef
  echeckoutPage <- liftIO (getCheckoutPage cfg invRef)
  case echeckoutPage of
    Left err -> do liftIO $ logError (show err)
                   checkoutErrorPage err
    Right checkoutPage -> renderTemplate cfg templ checkoutPage


routes :: Config -> Template -> ScottyM ()
routes cfg@Config{..} templates = do
  get  "/"                     (simplePage (t "home") ())
  get  "/signup"               (content signup)
  post "/signup"               postSignup
  get  "/:user"                (lookupUserPage cfg (t "user"))
  get  "/back/:user"           (tiersPage cfg (t "back"))
  post "/checkout/:tier_id"    (postCheckout cfg)
  get  "/checkout/:invoice_id" (getCheckout cfg (t "checkout"))
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

testServer :: IO ()
testServer = do
  putEnv "RPCSOCK=/home/jb55/.lightning-testnet-rpc"
  server

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
