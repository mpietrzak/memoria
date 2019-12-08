{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Memoria.Db (
    HasDbConn(getConnection, withConnection),
    HasDb(getDbSize, getSessionValue),
    createDbPool
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool (Pool, createPool, takeResource, withResource)
import Data.Text.Lazy (Text)
import Formatting ((%) , fixed, format, fprint, int, shown, text)
import Text.RawString.QQ
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

class MonadIO m => HasDbConn m where
    getConnection :: m (PSQL.Connection)
    withConnection :: (PSQL.Connection -> m b) -> m b

class HasDbConn m => HasDb m where
    getDbSize :: m (Either Text Integer)
    getSessionValue :: Text -> Text -> m (Maybe Text)

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
                    (HDBC.SqlRational size) -> pure $ Right $ round size
                    value -> pure $ Left $ format ("wrong type: " % shown) value
                 _ -> pure $ Left "more than one column in row"
              _ -> pure $ Left "more than one row"
    getSessionValue sessionId name = do
        let sql = [r|
                select value
                from
                    session
                    join session_value on (session_value.session = session.id)
                where
                    session.key = ?
                    and session_value.name = ?
            |]
        withConnection $ \conn -> do
            rows <- liftIO $ HDBC.quickQuery conn sql [HDBC.toSql sessionId, HDBC.toSql name]
            case rows of
              [row] -> case row of
                [sqlValue] -> case sqlValue of
                    (HDBC.SqlString value) -> pure $ Just $ Data.Text.Lazy.pack value
                    _ -> pure Nothing
                _ -> pure Nothing
              _ -> pure Nothing

createConnection :: Text -> Int -> Text -> Text -> Text -> IO (PSQL.Connection)
createConnection host port db user pass = do
    conn <- PSQL.connectPostgreSQL connstr
    fprint "createConnection: Created connection\n"
    pure conn
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

