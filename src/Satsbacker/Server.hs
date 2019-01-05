{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Server where

import Control.Applicative (optional)
import Control.Concurrent (MVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.SQLite.Simple
import Lucid
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Environment (lookupEnv)
import System.Posix.Env (putEnv)
import Text.Mustache
import Text.Read (readMaybe)
import Web.Scotty

import Invoicing

import Satsbacker.Config
import Satsbacker.Data.Checkout
import Satsbacker.Data.Email
import Satsbacker.Data.Invoice
import Satsbacker.Data.InvoiceId (InvId(..))
import Satsbacker.Data.Tiers
import Satsbacker.Data.TiersPage
import Satsbacker.Data.User
import Satsbacker.Html
import Satsbacker.Logging
import Satsbacker.Templates
import Database.SQLite.Table
import Crypto.Macaroons (Macaroon)
import Web.Cookie
import qualified Crypto.Macaroons as Macaroon


import qualified Data.HashMap.Lazy as Map
import qualified Data.Text.Lazy as LT


data Page = Dashboard

redirectTo page =
    let
        r = case page of
              Dashboard -> "/dashboard"
    in
      redirect r

getSignup :: Config -> Template -> ActionM ()
getSignup cfg@Config{..} templ = simplePage cfg templ

setAuthCookie :: Macaroon -> SetCookie
setAuthCookie macaroon =
  defaultSetCookie {
    setCookieName  = "session"
  , setCookieValue = Macaroon.serialize macaroon
  }

postSignup :: Config -> Template -> ActionM ()
postSignup cfg@Config{..} templ = do
  name  <- param "name"
  email <- param "email"
  pass  <- param "password"

  muser <- liftIO $ withMVar cfgConn $ \conn ->
    query conn "select id from users where name = ? or email = ?" (name, email)

  case muser of
    Just (Only id) ->
        signupError "A user with that username or email already exists"
    Nothing -> do sessionMacaroon cfgSecret
                  setAuthCookie m
                  redirectTo Dashboard
  where
    signupError msg = do
      renderTemplate config (merged "validation" () conf)


getTemplate :: Template -> PName -> Template
getTemplate templates pname =
    templates { templateActual = pname }


lookupUserPage :: Config -> Template -> ActionM ()
lookupUserPage cfg@Config{..} templ = do
  (userId, user) <- withUser cfgConn
  mstats <- liftIO $ withMVar cfgConn $ \conn -> getUserStats conn userId
  let stats = fromMaybe (UserStats 0 0) mstats
      userPage = UserPage user stats
  renderTemplate cfg templ userPage


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

newtype Merged a into = Merged { getMergedConfig :: ((Text, a), into) }

instance (ToJSON a, ToJSON into) => ToJSON (Merged a into) where
    toJSON (Merged ((key, obj), intoVal)) =
        let
            cfgObj v intoV = Object (Map.insert key v intoV)
        in
        case (toJSON intoVal, toJSON obj) of
          (Object intoV, valJson) -> cfgObj valJson intoV
          (_, valJson)            -> cfgObj valJson mempty -- don't merge if we see a non-object
merged key v into = Merged ((key, v), into)

renderTemplate :: ToJSON a => Config -> Template -> a -> ActionM ()
renderTemplate cfg templ val =
    let templateData = Merged (("config", cfg), val)
        templateJson = toJSON templateData
        (_w, rendered) = renderMustacheW templ templateJson
    in html rendered


postCheckout :: Config -> ActionM ()
postCheckout cfg@Config{..} = do
  tierId  <- fmap TierId   (param "tier_id")
  user    <- fmap Username (param "user")
  email   <- fmap Email    (param "email")
  payerId <- fmap (fmap UserId) (optional (param "payer_id"))
  einv <- liftIO $ mkCheckout cfg tierId
  case einv of
    Left err ->
        let desc = describeCheckoutError err
            uri  = "/back/" <> getUsername user <> "?err=" <> desc
        in redirect (LT.fromStrict uri)
    Right inv -> do let invId  = invoiceId inv
                        invRef = InvoiceRef invId tierId email payerId
                        coUri  = "/checkout/" <> getInvId (invoiceId inv)
                    _ <- liftIO $ withMVar cfgConn $ \conn ->
                           insert conn invRef
                    redirect (LT.fromStrict coUri)


getCheckout :: Config -> Template -> ActionM ()
getCheckout cfg@Config{..} templ = do
  invId   <- param "invoice_id"
  minvRef <- liftIO $ withMVar cfgConn $ \conn ->
               fetchOne conn (search "invoice_id" (invId :: Text))
  invRef  <- maybe (fail "checkout: could not find invoice id") return minvRef
  echeckoutPage <- liftIO (getCheckoutPage cfg invRef)
  case echeckoutPage of
    Left err -> do liftIO $ logError (show err)
                   checkoutErrorPage err
    Right checkoutPage -> renderTemplate cfg templ checkoutPage


simplePage :: Config -> Template -> ActionM ()
simplePage cfg t = renderTemplate cfg t ()


routes :: Config -> Template -> ScottyM ()
routes cfg@Config{..} templates = do
  get  "/"                     (simplePage cfg (t "home"))
  get  "/signup"               (getSignup cfg signupTemplate)
  post "/signup"               (postSignup cfg signupTemplate)
  get  "/:user"                (lookupUserPage cfg (t "user"))
  get  "/back/:user"           (tiersPage cfg (t "back"))
  post "/checkout/:tier_id"    (postCheckout cfg)
  get  "/checkout/:invoice_id" (getCheckout cfg (t "checkout"))
  invoiceRoutes cfg
  middleware (static "public")
  where
    signupTemplate = t "signup"
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
