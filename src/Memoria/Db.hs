{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Memoria.Db (
    HasDbConn(getConnection, withConnection),
    HasDb(getDbSize),
    createDbPool
) where

import Data.Pool (Pool, createPool, takeResource, withResource)
import Data.Text.Lazy (Text)
import Formatting ((%) , fixed, format, fprint, int, shown, text)
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.RawString.QQ

class MonadIO m => HasDbConn m where
    getConnection :: m (PSQL.Connection)
    withConnection :: (PSQL.Connection -> m b) -> m b

class HasDbConn m => HasDb m where
    getDbSize :: m (Either Text Text)
    getDbSize = do
        withConnection $ \conn -> do
            let sql = [r|
                    select sum(pg_total_relation_size(table_schema || '.' || table_name))
                    from information_schema.tables as t
                    where
                        t.table_schema = 'public'
                |]
            rows <- liftIO $ HDBC.quickQuery conn sql []
            case rows of
              [row] -> case row of
                 [sqlSize] -> case sqlSize of
                    (HDBC.SqlRational size) -> pure $ Right $ format (fixed 2) (round size)
                    value -> pure $ Left $ format ("wrong type: " % shown) value
                 _ -> pure $ Left "more than one column in row"
              _ -> pure $ Left "more than one row"

createConnection :: Text -> Int -> Text -> Text -> Text -> IO (PSQL.Connection)
createConnection host port db user pass = PSQL.connectPostgreSQL connstr
    where
        connstr = "user=" <> unpack user <> " "
            <> "password=" <> unpack pass <> " "
            <> "host=" <> unpack host <> " "
            <> "port=" <> (show port) <> " "
            <> "dbname=" <> unpack db <> " "
            <> "sslmode=prefer"
        unpack = Data.Text.Lazy.unpack

createDbPool :: Text -> Int -> Text -> Text -> Text -> IO (Pool PSQL.Connection)
createDbPool host port db user pass = createPool
    (createConnection host port db user pass)
    destroyConnection
    1
    10
    32

destroyConnection :: PSQL.Connection -> IO ()
destroyConnection conn = do
    HDBC.disconnect conn
    fprint "destroyConnection: Closed connection\n"

