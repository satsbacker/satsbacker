{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.SQLite.Table
    ( Table(..)
    , insert
    , insertL
    , fetchOne
    , fetchOneL
    , Search(..)
    , search
    , searchAny
    , withConn
    , Limit(..)
    , onlyOne
    , noLimit
    ) where

import Data.Text (Text)
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Control.Concurrent.MVar (MVar, withMVar)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

import qualified Data.Text as T

class Table a where
    tableName   :: a -> Text
    tableFields :: a -> [Text]

newtype Search a = Search { getSearch :: (Text, a) }
    deriving Show

newtype Limit = Limit { getLimit :: Int }
    deriving Show

onlyOne :: Limit
onlyOne = Limit 1

noLimit :: Limit
noLimit = Limit 0

search :: Text -> a -> Search a
search t v = Search (t,v)

searchAny :: Search Int
searchAny = Search ("1", 1)

insert :: (Table a, ToRow a) => Connection -> a -> IO Int
insert conn row = do
  let tfields = tableFields row
      fields = T.intercalate ", " tfields
      qs     = map (const "?") tfields
      qsc    = T.intercalate ", " qs
      q = "INSERT INTO "<>tableName row<>" ("<>fields<>") values ("<>qsc<>")"
  execute conn (Query q) row
  fmap fromIntegral (lastInsertRowId conn)

withConn :: MonadIO m => MVar a1 -> (a1 -> IO a2) -> m a2
withConn mvconn fn = liftIO (withMVar mvconn fn)
       
insertL :: (MonadIO m, Table a, ToRow a)
        => MVar Connection -> a -> m Int
insertL mvconn row = liftIO $ withMVar mvconn $ \conn -> insert conn row

fetchOne :: (Table a, FromRow a, ToField f, Show f)
         => Connection -> Search f -> IO (Maybe a)
fetchOne conn srch =
    fmap listToMaybe (fetch conn srch onlyOne)

fetchOneL :: (MonadIO m, Table a, FromRow a, ToField f, Show f)
          => MVar Connection -> Search f -> m (Maybe a)
fetchOneL mvconn srch =
  liftIO $ withMVar mvconn $ \conn -> fetchOne conn srch

fetch :: forall a f. (Table a, FromRow a, ToField f, Show f)
      => Connection -> Search f -> Limit -> IO [a]
fetch conn (Search (term, val)) (Limit lim) =
    let
        tn = tableName (undefined :: a)
        fs = tableFields (undefined :: a)
        fc = T.intercalate "," fs
        q = "SELECT "<>fc<>" FROM "<>tn
            <>" WHERE "<>term<>" = ? "
            <>(if lim == 0 then "" else " LIMIT " <> T.pack (show lim))
    in query conn (Query q) (Only val)
