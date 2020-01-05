{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Memoria.Db (
    HasDbConn(getConnection, withConnection),
    HasDb(createAccount, createSession, getDbSize, getSessionValue, setSessionValue),
    QuestionSet(..),
    createDbPool,
    createQuestionSet,
    sessionExists,
    getQuestionSet,
    getQuestionSetsForAccount
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool (Pool, createPool, takeResource, withResource)
import Data.Text.Lazy (Text)
import Formatting ((%) , fixed, format, fprint, int, shown, text)
import Text.RawString.QQ
import qualified Data.ByteString.Lazy as Data.ByteString.Lazy
import qualified Data.Text.Encoding.Error
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding as Data.Text.Lazy.Encoding
import qualified Data.UUID as Data.UUID
import qualified Data.UUID.V4 as Data.UUID.V4
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

data QuestionSet = QuestionSet { id :: Text, name :: Text }

class MonadIO m => HasDbConn m where
    getConnection :: m (PSQL.Connection)
    withConnection :: (PSQL.Connection -> m b) -> m b

class HasDbConn m => HasDb m where
    createAccount :: m Text
    createSession :: Text -> m ()
    getDbSize :: m (Either Text Integer)
    getSessionValue :: Text -> Text -> m (Maybe Text)
    setSessionValue :: Text -> Text -> Text -> m ()
    getQuestionSetsForAccount :: Text -> m [QuestionSet]

    createAccount = do
        accountId <- liftIO $ Data.UUID.V4.nextRandom >>= \uuid ->
            pure $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid
        withConnection $ \conn -> do
            let sql = [r|
                    insert into account (
                        id,
                        created_at,
                        modified_at
                    ) values (
                        ?,
                        current_timestamp,
                        current_timestamp
                    )
                |]
            liftIO $ HDBC.run conn sql [HDBC.toSql accountId]
            liftIO $ HDBC.commit conn
            pure accountId
    -- ID is a primary key. Key is a sessionId. ID is impl detail, key is a
    -- part of contract.
    createSession sessionKey = do
        id <- liftIO $ Data.UUID.V4.nextRandom >>= \uuid ->
            pure $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid
        withConnection $ \conn -> do
            let sql = [r|
                    insert into session (
                        id,
                        key,
                        created_at,
                        modified_at
                    ) values (
                        ?,
                        ?,
                        current_timestamp,
                        current_timestamp
                    )
                |]
            liftIO $ HDBC.run conn sql [HDBC.toSql id, HDBC.toSql sessionKey]
            liftIO $ HDBC.commit conn
            liftIO $ fprint
                ("Db.createSession: Created session with id " % text % " and key " % text % "\n")
                id
                sessionKey
            pure ()
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
    getQuestionSetsForAccount accountId = do
        let sql = [r|
                select
                    id,
                    name
                from
                    question_set
                where
                    owner = ?
            |]
        withConnection $ \conn -> do
            rows <- liftIO $ HDBC.quickQuery conn sql [HDBC.toSql accountId]
            pure $ map rowToQuestionSet rows
        where
            rowToQuestionSet row = case row of
                [HDBC.SqlByteString id, HDBC.SqlByteString name] ->
                   QuestionSet { id = bsToText id, name = bsToText name }
                _ -> error "Invalid type"
            bsToText = Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
                . Data.ByteString.Lazy.fromStrict

    getSessionValue sessionKey name = do
        liftIO $ fprint
            ("getSessionValue: Querying session '" % text % "' for '" % text % "'\n")
            sessionKey
            name
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
            rows <- liftIO $ HDBC.quickQuery conn sql [HDBC.toSql sessionKey, HDBC.toSql name]
            case rows of
              [row] -> case row of
                [sqlValue] -> case sqlValue of
                    (HDBC.SqlByteString value) ->
                        pure
                        $ Just
                        $ Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
                        $ Data.ByteString.Lazy.fromStrict value
                    othertype -> error ("Unexpected type: " <> show othertype)
                    _ -> pure Nothing
                _ -> pure Nothing
              _ -> pure Nothing
    setSessionValue sessionKey name value = do
        liftIO $ fprint
            ("Db.setSessionValue: About to insert value (" % text % ", " % text % " to session " % text % "\n")
            name
            value
            sessionKey
        id <- liftIO $ Data.UUID.V4.nextRandom >>= \uuid ->
            pure $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid
        let sql = [r|
                insert into session_value (
                    id,
                    session,
                    name,
                    value,
                    created_at,
                    modified_at
                ) values (
                    ?,
                    (select id from session where key = ?),
                    ?,
                    ?,
                    current_timestamp,
                    current_timestamp
                ) on conflict (session, name)
                    do update set value = ?, modified_at = current_timestamp;
            |]
        withConnection $ \conn -> do
            liftIO $ HDBC.run conn sql [
                    HDBC.toSql id,
                    HDBC.toSql sessionKey,
                    HDBC.toSql name,
                    HDBC.toSql value,
                    HDBC.toSql value
                ]
            liftIO $ HDBC.commit conn
        liftIO $ fprint ("Db.setSessionValue: Session value saved in DB\n")
        pure ()

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

createQuestionSet :: (HasDbConn m, Monad m, MonadIO m) => Text -> Text -> Text -> m ()
createQuestionSet id name owner = do
    withConnection $ \conn -> do
        let sql = [r|
            insert into question_set (id, name, owner, created_at, modified_at)
            values (?, ?, ?, current_timestamp, current_timestamp)
            |]
        liftIO $ HDBC.run conn sql [ HDBC.toSql id
                                   , HDBC.toSql name
                                   , HDBC.toSql owner ]
        liftIO $ HDBC.commit conn


destroyConnection :: PSQL.Connection -> IO ()
destroyConnection conn = do
    HDBC.disconnect conn
    fprint "destroyConnection: Closed connection\n"

getQuestionSet :: (HasDbConn m, MonadIO m) => Text -> Text -> m QuestionSet
getQuestionSet owner id = do
    withConnection $ \conn -> do
        rows <- liftIO $ HDBC.quickQuery conn sql [HDBC.toSql owner, HDBC.toSql id]
        case rows of
            [row] -> pure $ rowToQuestionSet row
            _ -> error "Invalid number of rows returned from DB"
    where
        rowToQuestionSet row = case row of
            [sqlId, sqlName] -> QuestionSet { id = HDBC.fromSql sqlId
                                            , name = HDBC.fromSql sqlName }
            _ -> error "Invalid number of columns"
        sql = [r|
            select id, name
            from question_set
            where
                owner = ?
                and id = ?
        |]

sessionExists :: (Monad m, HasDb m) => Text -> m Bool
sessionExists sessionKey = do
    withConnection $ \conn -> do
        let sql = [r|
                select created_at
                from session
                where key = ?
            |]
        let params = [HDBC.toSql sessionKey]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        case rows of
            [row] -> case row of
                [HDBC.SqlString v] -> pure True
                [HDBC.SqlLocalTime v] -> pure True
                v -> do
                    liftIO $ fprint ("sessionExists: Unexpected type: " % shown % "\n") v
                    pure True
            _ -> pure False
