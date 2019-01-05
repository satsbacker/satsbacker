{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Server where

import Control.Applicative (optional)
import Control.Concurrent (MVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Database.SQLite.Simple
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Environment (lookupEnv)
import System.Posix.Env (putEnv)
import Text.Mustache
import Text.Read (readMaybe)
import Web.Scotty

import Invoicing

import Database.SQLite.Table
import Satsbacker.Config
import Satsbacker.Cookies
import Satsbacker.Data.Checkout
import Satsbacker.Data.Email
import Satsbacker.Email
import Satsbacker.Data.Invoice
import Satsbacker.Data.InvoiceId (InvId(..))
import Satsbacker.Data.Tiers
import Satsbacker.Data.TiersPage
import Satsbacker.Data.User
import Satsbacker.Logging
import Satsbacker.Macaroons
import Satsbacker.Templates

import qualified Data.Text.Lazy as LT


data Page = Dashboard

redirectTo :: Page -> ActionM a
redirectTo page =
    let
        r = case page of
              Dashboard -> "/dashboard"
    in
      redirect r


getSignup :: Config -> Template -> ActionM ()
getSignup cfg@Config{..} templ = simplePage cfg templ


registerUser :: MVar Connection -> Username -> Email -> Plaintext -> IO (UserId, User)
registerUser mvconn name email pass = do
  u <- createUser pass
  let user = u { userName = name
               , userEmail = email
               }
  userId <- UserId <$> withMVar mvconn (\conn -> insert conn user)
  return (userId, user)


postSignup :: Config -> Template -> ActionM ()
postSignup cfg@Config{..} templ = do
  name  <- fmap Username (param "name")
  email <- fmap Email (param "email")
  pass  <- fmap Plaintext (param "password")

  muser <- liftIO $ fmap listToMaybe $ withMVar cfgConn $ \conn ->
    query conn "select id from users where name = ? or email = ? limit 1" (name, email)

  case muser of
    Just (Only (_id :: Int)) ->
      signupError "A user with that username or email already exists"
    Nothing -> do (userId, user) <- liftIO (registerUser cfgConn name email pass)
                  liftIO (signupEmail cfg user email templ)
                  m <- liftIO (sessionMacaroon cfgSecret userId)
                  setCookie (authCookie m)
                  redirectTo Dashboard
  where
    signupError :: Text -> ActionM ()
    signupError msg = do
      let validation = object [ "error" .= msg ]
      renderTemplateM cfg templ (merged "validation" validation cfg)


lookupUserPage :: Config -> Template -> ActionM ()
lookupUserPage cfg@Config{..} templ = do
  (userId, user) <- withUser cfgConn
  mstats <- liftIO $ withMVar cfgConn $ \conn -> getUserStats conn userId
  let stats = fromMaybe (UserStats 0 0) mstats
      userPage = UserPage user stats
  renderTemplateM cfg templ userPage


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
  renderTemplateM cfg templ tpage


checkoutErrorPage :: CheckoutError -> ActionM ()
checkoutErrorPage = raise . describeCheckoutError


merged :: Text -> a -> into -> Merged a into
merged key v into = Merged ((key, v), into)


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
    Right checkoutPage -> renderTemplateM cfg templ checkoutPage


simplePage :: Config -> Template -> ActionM ()
simplePage cfg t = renderTemplateM cfg t ()


routes :: Config -> Template -> ScottyM ()
routes cfg@Config{..} templates = do
  get  "/"                     (simplePage cfg (t "home"))
  get  "/dashboard"            (simplePage cfg (t "dashboard"))
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
