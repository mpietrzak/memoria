{-# LANGUAGE OverloadedStrings #-}

module Memoria.Db (
    HasDbConn(getConnection, withConnection),
    HasDb(getDbSize),
    createDbPool
) where

import Data.Pool (Pool, createPool, takeResource, withResource)
import Data.Text.Lazy (Text)
import Formatting ((%) , format, fprint, int, shown, text)
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

class Monad m => HasDbConn m where
    getConnection :: m (PSQL.Connection)
    withConnection :: (PSQL.Connection -> m b) -> m b

class HasDbConn m => HasDb m where
    getDbSize :: m (Either Text Text)
    getDbSize = pure $ Right "test"

createConnection :: Text -> Int -> Text -> Text -> Text -> IO (PSQL.Connection)
createConnection host port db user pass = PSQL.connectPostgreSQL connstr
    where
        connstr = "user=" <> unpack user <> " "
            <> "password=" <> unpack pass <> " "
            <> "host=" <> unpack host <> " "
            <> "port=" <> (show port) <> " "
            <> "dbname=" <> unpack db <> " "
            <> "sslmode=require"
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

