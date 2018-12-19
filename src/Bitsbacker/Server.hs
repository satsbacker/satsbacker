{-# LANGUAGE RecordWildCards #-}

module Bitsbacker.Server where

import Control.Concurrent (MVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Foldable (foldl')
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

import Invoicing

import Bitsbacker.Templates
import Bitsbacker.Config
import Bitsbacker.Data.User
import Bitsbacker.Data.Tiers
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
  muser <- liftIO $ getUser mvconn (Username username)
  case muser of
    Nothing -> next
    Just user -> return user

data TiersPage = TiersPage {
      tiersPageUser   :: User
    , tiersRows       :: [TierCols]
    , tiersColumnWidth :: Text
    , tiersNumColumns  :: Int
    }

instance ToJSON TiersPage where
    toJSON TiersPage{..} =
        object [ "tiers"    .= tiersRows
               , "user"     .= tiersPageUser
               , "colwidth" .= tiersColumnWidth
               , "ncolumns" .= tiersNumColumns
               ]

mkTiersPage :: User -> Int -> [Tier] -> TiersPage
mkTiersPage user cols tiers =
  TiersPage {
    tiersPageUser   = user
  , tiersRows       = map TierCols rs
  , tiersNumColumns = cols
  , tiersColumnWidth =
      case cols of
        2 -> "one-half"
        3 -> "one-third"
        4 -> "one-fourth"
        _ -> ""
  }
  where
    rs :: [[Tier]]
    rs = foldr folder [] tiers
    folder tier rows =
        case rows of
          [] -> [[tier]]
          row:rest
            | length row == cols -> [tier] : row : rest
            | otherwise          -> (tier:row) : rest



tiersPage :: MVar Connection -> Template -> ActionM ()
tiersPage mvconn templ = do
  (userId, user) <- withUser mvconn
  tiers <- liftIO (getTiers mvconn userId)
  let tpage = mkTiersPage user 2 tiers
      (_warnings, rendered) = renderMustacheW templ (toJSON tpage)
  html rendered


routes :: Config -> Template -> ScottyM ()
routes cfg@Config{..} templates = do
  get  "/"           (simplePage (t "home") ())
  get  "/signup"     (content signup)
  post "/signup"     postSignup
  get  "/:user"      (lookupUserPage cfgConn (t "user"))
  get  "/back/:user" (tiersPage cfgConn (t "back"))
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
