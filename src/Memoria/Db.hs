{-# LANGUAGE OverloadedStrings #-}

module Memoria.Db (
    HasConnections,
    createDbPool
) where

import Data.Text.Lazy (Text)
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL
import qualified Data.Text.Lazy as Data.Text.Lazy
import Data.Pool (Pool, createPool, takeResource, withResource)
import Formatting ((%) , format, fprint, int, shown, text)

class HasConnections m where
    getConnection :: m (PSQL.Connection)
    withConnection :: (PSQL.Connection -> m b) -> m b

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



