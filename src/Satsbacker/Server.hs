{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Server where

import Control.Applicative (optional)
import Control.Concurrent (MVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Unsafe.Coerce (unsafeCoerce)
import Data.Bool (bool)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Database.SQLite.Simple
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Posix.Env (putEnv)
import Web.Scotty

import Invoicing

import Database.SQLite.Table
import Satsbacker.Config
import Satsbacker.Cookies
import Satsbacker.Data.Checkout
import Satsbacker.Data.Email
import Satsbacker.Data.Invoice
import Satsbacker.Data.InvoiceId (InvId(..))
import Satsbacker.Data.Merged
import Satsbacker.Data.Site (Site(..))
import Satsbacker.Data.Tiers
import Satsbacker.Data.TiersPage
import Satsbacker.Data.User
import Satsbacker.Email
import Satsbacker.Logging
import Satsbacker.Macaroons
import Satsbacker.Templates.Render

import qualified Data.Text.Lazy as LT
import qualified Crypto.Macaroons as Macaroon


data Page = Dashboard

redirectTo :: Page -> ActionM a
redirectTo page =
    let
        r = case page of
              Dashboard -> "/dashboard"
    in
      redirect r


getSignup :: Config -> ActionM ()
getSignup cfg = simplePage cfg "signup"


registerUser :: MVar Connection -> Username -> Email -> Plaintext -> IO (UserId, User)
registerUser mvconn name email pass = do
  u <- createUser pass
  let user = u { userName = name
               , userEmail = email
               }
  userId <- UserId <$> withMVar mvconn (\conn -> insert conn user)
  return (userId, user)


postSignup :: Config -> ActionM ()
postSignup cfg@Config{..} = do
  name  <- fmap Username (param "name")
  email <- fmap Email (param "email")
  pass  <- fmap Plaintext (param "password")

  muser <- liftIO $ fmap listToMaybe $ withMVar cfgConn $ \conn ->
    query conn "select id from users where name = ? or email = ? limit 1" (name, email)

  case muser of
    Just (Only (_id :: Int)) ->
      signupError "A user with that username or email already exists"
    Nothing -> do (userId, user) <- liftIO (registerUser cfgConn name email pass)
                  emailMac <- liftIO (mintEmailMacaroon cfgSecret email userId)
                  liftIO (signupEmail cfg user emailMac email)
                  m <- liftIO (sessionMacaroon cfgSecret userId)
                  setCookie (authCookie m)
                  redirectTo Dashboard
  where
    signupError :: Text -> ActionM ()
    signupError msg = do
      let validation = object [ "error" .= msg ]
      renderTemplateM cfg "signup" (merged "validation" validation cfg)


lookupUserPage :: Config -> ActionM ()
lookupUserPage cfg@Config{..} = do
  (userId, user) <- withUser cfgConn
  mstats <- liftIO $ withMVar cfgConn $ \conn -> getUserStats conn userId
  let stats    = fromMaybe (UserStats 0 0) mstats
      userPage = UserPage user stats
      acfg     = siteAmountCfg cfgSite
  renderTemplateM cfg "user" (userPageToJSON acfg userPage)


withUser :: MVar Connection -> ActionM (UserId, User)
withUser mvconn = do
  username <- param "user"
  muser <- liftIO $ withMVar mvconn $ \conn -> getUser conn (Username username)
  case muser of
    Nothing -> next
    Just user -> return user


tiersPage :: Config -> ActionM ()
tiersPage cfg@Config{..} = do
  (userId, user) <- withUser cfgConn
  tiers <- liftIO $ withMVar cfgConn $ \conn -> getTiers conn userId
  let tpage = mkTiersPage user 2 tiers
      acfg  = siteAmountCfg cfgSite
  renderTemplateM cfg "back" (tiersPageToJSON acfg tpage)


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


getCheckout :: Config -> ActionM ()
getCheckout cfg@Config{..} = do
  invId   <- param "invoice_id"
  minvRef <- liftIO $ withMVar cfgConn $ \conn ->
               fetchOne conn (search "invoice_id" (invId :: Text))
  invRef  <- maybe (fail "checkout: could not find invoice id") return minvRef
  echeckoutPage <- liftIO (getCheckoutPage cfg invRef)
  case echeckoutPage of
    Left err -> do liftIO $ logError (show err)
                   checkoutErrorPage err
    Right checkoutPage ->
      let acfg = siteAmountCfg cfgSite
      in renderTemplateMV cfg "checkout" (checkoutPageToJSON acfg checkoutPage)


simplePage :: Config -> Text -> ActionM ()
simplePage cfg t = simplePage_ cfg t ()

simplePage_ :: ToJSON a => Config -> Text -> a  -> ActionM ()
simplePage_ cfg@Config{..} t = renderTemplateM cfg t


singleQuery :: Functor f => f [Only b] -> f (Maybe b)
singleQuery = fmap (fmap fromOnly . listToMaybe)


errorPage :: forall a. Config -> Text -> ActionM a
errorPage cfg@Config{..} err =
    fmap unsafeCoerce (simplePage_ cfg "error" err)

setEmailConfirmed :: Connection -> UserId -> IO ()
setEmailConfirmed conn (UserId userId) = do
  execute conn "update users set (email_confirmed) = 1 where id = ?"
          (Only userId)

assocSubscriptions :: Connection -> Email -> UserId -> IO ()
assocSubscriptions conn (Email email) (UserId userId) =
  execute conn "update subscriptions set (user_id) = (?) where user_email = ?"
          (userId, email)


confirmEmail :: Config -> ActionM ()
confirmEmail cfg@Config{..} = do
  macaroonStr <- param "macaroon"

  let mmacaroon = Macaroon.deserialize macaroonStr
      muid      = either (const Nothing) macaroonUserId mmacaroon

  macaroon <- either (const (errPage "invalid macaroon")) return mmacaroon
  userId   <- maybe (errPage "missing userId in macaroon") return muid

  let mvalid = Macaroon.isValidSig cfgSecret macaroon
      uid    = getUserId userId

  bool (fail "invalid macaroon signature") (return ()) mvalid
  memail <- singleQuery $ liftIO $ withMVar cfgConn $ \conn ->
    query conn "select email from users where id = ? limit 1" (Only uid)

  email <- maybe (errPage "could not find") return memail
  let email_ = Email email
  ok <- liftIO (verifyEmailMacaroon cfgSecret email_ userId macaroon)
  bool invalidToken (finalize email_ userId) ok
  where
    finalize email userId = do
      liftIO $ withMVar cfgConn $ \conn -> setEmailConfirmed conn userId
      liftIO $ withMVar cfgConn $ \conn -> assocSubscriptions conn email userId
      redirectTo Dashboard

    errPage :: forall a. Text -> ActionM a
    errPage = errorPage cfg

    invalidToken = errPage "Invalid or expired email confirmation"



routes :: Config -> ScottyM ()
routes cfg@Config{..} = do
  get  "/"                        (simplePage cfg "home")
  get  "/dashboard"               (simplePage cfg "dashboard")
  get  "/confirm-email/:macaroon" (confirmEmail cfg)
  get  "/signup"                  (getSignup cfg)
  post "/signup"                  (postSignup cfg)
  get  "/:user"                   (lookupUserPage cfg)
  get  "/back/:user"              (tiersPage cfg)
  post "/checkout/:tier_id"       (postCheckout cfg)
  get  "/checkout/:invoice_id"    (getCheckout cfg)
  invoiceRoutes cfg
  middleware (static "public")


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
  port <- getPort
  scotty port (routes cfg)
