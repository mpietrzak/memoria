-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Memoria.Db
    ( HasDbConn(withConnection)
    , HasDb(createAccount, createSession, getDbSize, getSessionValue,
      setSessionValue)
    , Account(..)
    , AccountEmail(..)
    , Answer(..)
    , QuestionSet(..)
    , Question(..)
    , addEmail
    , addLoginToken
    , addQuestion
    , addQuestionAnswer
    , createDbPool
    , createQuestionSet
    , deleteSessionValue
    , ensureCsrfToken
    , getAccountEmails
    , getAccountIdByToken
    , getAccountsByEmail
    , getAllQuestionsForAccount
    , getAnswerById
    , getAnswers
    , getQuestionById
    , getQuestionSet
    , getQuestionSetById
    , getQuestionSetQuestions
    , getQuestionSetsForAccount
    , getRandomQuestion
    , getSubscribedQuestionSetsForAccount
    , searchQuestionSets
    , sessionExists
    , setQuestionSetDeleted
    , subscribeQuestionSet
    , updateAnswer
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random.Class (weighted)
import qualified Data.Bifunctor
import qualified Data.ByteString.Lazy as Data.ByteString.Lazy
import qualified Data.Map.Lazy
import qualified Data.Maybe
import Data.Pool (Pool, createPool, takeResource, withResource)
import qualified Data.Text.Encoding.Error
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding as Data.Text.Lazy.Encoding
import qualified Data.UUID as Data.UUID
import qualified Data.UUID.V4 as Data.UUID.V4
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL
import Formatting ((%), (%.), fixed, format, fprint, int, left, right, shown, text)
import Prelude hiding (id)
import Text.RawString.QQ

data Account =
    Account
        { aId :: Text
        , aEmails :: [AccountEmail]
        , aCreatedAt :: Text
        , aModifiedAt :: Text
        }

data AccountEmail =
    AccountEmail
        { aeId :: Text
        , aeEmail :: Text
        , aeCreatedAt :: Text
        , aeModifiedAt :: Text
        }

data Answer =
    Answer
        { ansId :: Text
        , ansQuestionId :: Text
        , ansAnswer :: Text
        , ansIsCorrect :: Bool
        , ansAnsweredAt :: Text
        , ansCreatedAt :: Text
        , ansModifiedAt :: Text
        }

data QuestionSet =
    QuestionSet
        { qsId :: Text
        , qsName :: Text
        , qsOwner :: Text
        , qsCreatedAt :: Text
        , qsModifiedAt :: Text
        }

data Question =
    Question
        { qId :: Text
        , qQuestion :: Text
        , qAnswer :: Text
        , qCreatedAt :: Text
        , qModifiedAt :: Text
        , qScore :: Double
        }
    deriving (Show)

class MonadIO m =>
      HasDbConn m
    where
    withConnection :: (PSQL.Connection -> m b) -> m b

class HasDbConn m =>
      HasDb m
    where
    createAccount :: m Text
    createSession :: Text -> m ()
    getDbSize :: m (Either Text Integer)
    getSessionValue :: Text -> Text -> m (Maybe Text)
    setSessionValue :: Text -> Text -> Text -> m ()
    getQuestionSetsForAccount :: Text -> m [QuestionSet]
    createAccount = do
        accountId <- newId
        withConnection $ \conn -> do
            let sql =
                    [r|
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
        id <- newId
        withConnection $ \conn -> do
            let sql =
                    [r|
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
            liftIO $
                fprint
                    ("Db.createSession: Created session with id " % text % " and key " % text % "\n")
                    id
                    sessionKey
            pure ()
    getDbSize = do
        withConnection $ \conn -> do
            let sql =
                    [r|
                    select sum(pg_total_relation_size(table_schema || '.' || table_name))
                    from information_schema.tables as t
                    where
                        t.table_schema = 'public'
                |]
            rows <- liftIO $ HDBC.quickQuery conn sql []
            case rows of
                [row] ->
                    case row of
                        [sqlSize] ->
                            case sqlSize of
                                (HDBC.SqlRational size) -> pure $ Right $ round size
                                value -> pure $ Left $ format ("wrong type: " % shown) value
                        _ -> pure $ Left "more than one column in row"
                _ -> pure $ Left "more than one row"
    getQuestionSetsForAccount accountId = do
        let sql =
                [r|
                select
                    id,
                    name,
                    owner,
                    created_at,
                    modified_at
                from
                    question_set
                where
                    owner = ?
                    and (is_deleted = false or is_deleted is null)
            |]
        withConnection $ \conn -> do
            rows <- liftIO $ HDBC.quickQuery conn sql [HDBC.toSql accountId]
            pure $ map rowToQuestionSet rows
      where
        rowToQuestionSet row =
            case row of
                [id, name, owner, createdAt, modifiedAt] ->
                    QuestionSet
                        { qsId = HDBC.fromSql id
                        , qsName = HDBC.fromSql name
                        , qsOwner = HDBC.fromSql owner
                        , qsCreatedAt = HDBC.fromSql createdAt
                        , qsModifiedAt = HDBC.fromSql modifiedAt
                        }
                _ -> error "Invalid type"
    getSessionValue sessionKey name = do
        let sql =
                [r|
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
                [row] ->
                    case row of
                        [sqlValue] ->
                            case sqlValue of
                                (HDBC.SqlByteString value) ->
                                    pure $
                                    Just $
                                    Data.Text.Lazy.Encoding.decodeUtf8With
                                        Data.Text.Encoding.Error.lenientDecode $
                                    Data.ByteString.Lazy.fromStrict value
                                othertype -> error ("Unexpected type: " <> show othertype)
                                _ -> pure Nothing
                        _ -> pure Nothing
                _ -> pure Nothing
    setSessionValue sessionKey name value = do
        id <- newId
        let sql =
                [r|
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
                    do update set value = ?, modified_at = current_timestamp
            |]
        withConnection $ \conn -> do
            liftIO $
                HDBC.run
                    conn
                    sql
                    [ HDBC.toSql id
                    , HDBC.toSql sessionKey
                    , HDBC.toSql name
                    , HDBC.toSql value
                    , HDBC.toSql value
                    ]
            liftIO $ HDBC.commit conn
        liftIO $ fprint ("Db.setSessionValue: Session value saved in DB\n")
        pure ()

addEmail :: (HasDbConn m, MonadIO m) => Text -> Text -> m ()
addEmail accId email = do
    emailId <- newId
    withConnection $ \conn -> do
        let sql =
                [r|
                    insert into account_email (
                        id,
                        account,
                        email,
                        created_at,
                        modified_at
                    ) values (
                        ?,
                        ?,
                        ?,
                        current_timestamp,
                        current_timestamp
                    )
                |]
        let params = [HDBC.toSql emailId, HDBC.toSql accId, HDBC.toSql email]
        liftIO $ HDBC.run conn sql params
        liftIO $ HDBC.commit conn

addLoginToken :: (HasDbConn m, MonadIO m) => Text -> m Text
addLoginToken accId = do
    newLoginToken <-
        liftIO $
        Data.UUID.V4.nextRandom >>= \uuid ->
            pure $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid
    newLoginTokenId <- newId
    withConnection $ \conn -> do
        let params = [HDBC.toSql newLoginTokenId, HDBC.toSql newLoginToken, HDBC.toSql accId]
        liftIO $ HDBC.run conn sql params
        liftIO $ HDBC.commit conn
        pure newLoginToken
  where
    sql =
        [r|
                insert into login_token (
                    id,
                    token,
                    account,
                    expires_at,
                    created_at,
                    modified_at
                ) values (
                    ?,
                    ?,
                    ?,
                    current_timestamp + interval '7 days',
                    current_timestamp,
                    current_timestamp
                )
            |]

addQuestion :: (HasDbConn m, MonadIO m) => Text -> Text -> (Text, Text) -> m Text
addQuestion accId questionSetId (question, answer) = do
    withConnection $ \conn -> do
        questionId <- newId
        let sql =
                [r|
                    insert into question (
                        id,
                        question_set,
                        question,
                        answer,
                        created_at,
                        modified_at
                    ) values (
                        ?,
                        (
                            select
                                id
                            from
                                question_set
                            where
                                owner = ?
                                and id = ?
                        ),
                        ?,
                        ?,
                        current_timestamp,
                        current_timestamp
                    )
                |]
        liftIO $
            HDBC.run
                conn
                sql
                [ HDBC.toSql questionId
                , HDBC.toSql accId
                , HDBC.toSql questionSetId
                , HDBC.toSql question
                , HDBC.toSql answer
                ]
        liftIO $ HDBC.commit conn
        pure questionId

addQuestionAnswer :: (HasDbConn m, MonadIO m) => Text -> Text -> Text -> Bool -> m Text
addQuestionAnswer accId questionId answer isCorrect = do
    qaId <- newId
    withConnection $ \conn -> do
        let params =
                [ HDBC.toSql qaId
                , HDBC.toSql accId
                , HDBC.toSql questionId
                , HDBC.toSql answer
                , HDBC.toSql isCorrect
                ]
        liftIO $ HDBC.run conn sql params
        liftIO $ HDBC.commit conn
    pure qaId
  where
    sql =
        [r|
                insert into question_answer (
                    id,
                    account,
                    question,
                    answer,
                    is_correct,
                    answered_at,
                    created_at,
                    modified_at
                ) values (
                    ?,
                    ?,
                    ?,
                    ?,
                    ?,
                    current_timestamp,
                    current_timestamp,
                    current_timestamp
                )
            |]

createConnection :: Text -> Int -> Text -> Text -> Text -> IO (PSQL.Connection)
createConnection host port db user pass = do
    conn <- PSQL.connectPostgreSQL connstr
    fprint "createConnection: Created connection\n"
    pure conn
  where
    connstr =
        "user=" <>
        unpack user <>
        " " <>
        "password=" <>
        unpack pass <>
        " " <>
        "host=" <>
        unpack host <>
        " " <> "port=" <> (show port) <> " " <> "dbname=" <> unpack db <> " " <> "sslmode=prefer"
    unpack = Data.Text.Lazy.unpack

createDbPool :: Text -> Int -> Text -> Text -> Text -> IO (Pool PSQL.Connection)
createDbPool host port db user pass =
    createPool (createConnection host port db user pass) destroyConnection 1 10 32

createQuestionSet :: (HasDbConn m, Monad m, MonadIO m) => Text -> Text -> Text -> m ()
createQuestionSet id name owner = do
    withConnection $ \conn -> do
        let sql =
                [r|
            insert into question_set (id, name, owner, created_at, modified_at)
            values (?, ?, ?, current_timestamp, current_timestamp)
            |]
        liftIO $ HDBC.run conn sql [HDBC.toSql id, HDBC.toSql name, HDBC.toSql owner]
        liftIO $ HDBC.commit conn

destroyConnection :: PSQL.Connection -> IO ()
destroyConnection conn = do
    HDBC.disconnect conn
    fprint "destroyConnection: Closed connection\n"

getAccountIdByToken :: HasDbConn m => Text -> m (Maybe Text)
getAccountIdByToken token = do
    withConnection $ \conn -> do
        let params = [HDBC.toSql token]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        case rows of
            [row] ->
                case row of
                    [sqlAccId] -> pure $ Just $ HDBC.fromSql sqlAccId
                    _ -> error "Invalid number of columns"
            _ -> pure $ Nothing
  where
    sql =
        [r|
                select account
                from login_token
                where token = ?
                    and expires_at >= current_timestamp
            |]

getAccountEmails :: (HasDbConn m) => Text -> m [AccountEmail]
getAccountEmails accId = do
    withConnection $ \conn -> do
        let sql =
                [r|
                    select id, email, created_at, modified_at
                    from account_email
                    where account = ?
                    order by id
                |]
        let params = [HDBC.toSql accId]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        pure $ map rowToAccEmail rows
  where
    rowToAccEmail row =
        case row of
            [id, email, createdAt, modifiedAt] ->
                AccountEmail
                    { aeId = HDBC.fromSql id
                    , aeEmail = HDBC.fromSql email
                    , aeCreatedAt = HDBC.fromSql createdAt
                    , aeModifiedAt = HDBC.fromSql modifiedAt
                    }

getAccountsByEmail :: (HasDbConn m) => Text -> m [Account]
getAccountsByEmail email
    -- Select all accounts linked to this email, then select all emails connected
    -- to those accounts.
 = do
    let sql =
            [r|
                select
                    account.id,
                    account.created_at,
                    account.modified_at,
                    account_email.id,
                    account_email.email,
                    account_email.created_at,
                    account_email.modified_at
                from
                    account
                    join account_email on (account_email.account = account.id)
                where
                    account.id in (
                        select account_email.account
                        from account_email
                        where account_email.email = ?
                    )
            |]
    let params = [HDBC.toSql email]
    withConnection $ \conn -> do
        rows <- liftIO $ HDBC.quickQuery conn sql params
        let accountsWithEmails = foldRows Data.Map.Lazy.empty rows
        pure $ Data.Map.Lazy.elems accountsWithEmails
  where
    foldRows accMap rows =
        case rows of
            [] -> accMap
            row:rest ->
                case row of
                    [sqlAccId, sqlAccCreatedAt, sqlAccModifiedAt, sqlAccEmailId, sqlAccEmailEmail, sqlAccEmailCreatedAt, sqlAccEmailModifiedAt] -> do
                        let accId = HDBC.fromSql sqlAccId
                        let accCreatedAt = HDBC.fromSql sqlAccCreatedAt
                        let accModifiedAt = HDBC.fromSql sqlAccModifiedAt
                        let accEmailId = HDBC.fromSql sqlAccEmailId
                        let accEmailEmail = HDBC.fromSql sqlAccEmailEmail
                        let accEmailCreatedAt = HDBC.fromSql sqlAccEmailCreatedAt
                        let accEmailModifiedAt = HDBC.fromSql sqlAccEmailModifiedAt
                        let accountEmail =
                                AccountEmail
                                    { aeId = accEmailId
                                    , aeEmail = accEmailEmail
                                    , aeCreatedAt = accEmailCreatedAt
                                    , aeModifiedAt = accEmailModifiedAt
                                    }
                        let account =
                                Data.Maybe.fromMaybe
                                    Account
                                        { aId = accId
                                        , aCreatedAt = accCreatedAt
                                        , aModifiedAt = accModifiedAt
                                        , aEmails = []
                                        }
                                    (Data.Map.Lazy.lookup accId accMap)
                        let newAccount = account {aEmails = accountEmail : aEmails account}
                        let newAccMap = Data.Map.Lazy.insert accId newAccount accMap
                        foldRows newAccMap rest

getAllQuestionsForAccount :: (HasDbConn m, MonadIO m) => Text -> m [Question]
getAllQuestionsForAccount accId = do
    withConnection $ \conn -> do
        let params = [HDBC.toSql accId]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        pure $ map rowToQuestion rows
  where
    rowToQuestion row =
        case row of
            [id, question, answer, createdAt, modifiedAt] ->
                Question
                    { qId = HDBC.fromSql id
                    , qQuestion = HDBC.fromSql question
                    , qAnswer = HDBC.fromSql answer
                    , qCreatedAt = HDBC.fromSql createdAt
                    , qModifiedAt = HDBC.fromSql modifiedAt
                    }
    sql =
        [r|
                select id, question, answer, created_at, modified_at
                from question
                where question_set in (select id from question_set where owner = ?)
                order by id
            |]

getAnswerById :: (HasDbConn m, MonadIO m) => Text -> Text -> m (Maybe Answer)
getAnswerById accId ansId =
    withConnection $ \conn -> do
        let params = [HDBC.toSql ansId, HDBC.toSql accId, HDBC.toSql accId, HDBC.toSql accId]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        case rows of
            [] -> pure Nothing
            [row] -> do
                case row of
                    [id, answer, isCorrect, answeredAt, createdAt, modifiedAt, questionId] ->
                        pure $
                        Just $
                        Answer
                            { ansId = HDBC.fromSql id
                            , ansAnswer = HDBC.fromSql answer
                            , ansIsCorrect = HDBC.fromSql isCorrect
                            , ansAnsweredAt = HDBC.fromSql answeredAt
                            , ansCreatedAt = HDBC.fromSql answeredAt
                            , ansModifiedAt = HDBC.fromSql modifiedAt
                            , ansQuestionId = HDBC.fromSql questionId
                            }
                    _ -> error "Invalid columns"
            _ -> error "Too many rows"
  where
    sql =
        [r|
                select id, answer, is_correct, answered_at, created_at, modified_at, question
                from question_answer
                where
                    id = ?
                    and account = ?
                    and question in (
                        select id
                        from question
                        where question_set in (
                            select id
                            from question_set
                            where owner = ?
                            union all
                            select question_set
                            from question_set_subscription
                            where account = ?
                        )
                    )
            |]

getAnswers :: (HasDbConn m, MonadIO m) => Text -> Text -> m [Answer]
getAnswers accId questionId =
    withConnection $ \conn -> do
        let params = [HDBC.toSql accId, HDBC.toSql questionId]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        pure $ map rowToAnswer rows
  where
    rowToAnswer =
        \case
            [id, answer, isCorrect, answeredAt, createdAt, modifiedAt] ->
                Answer
                    { ansId = HDBC.fromSql id
                    , ansAnswer = HDBC.fromSql answer
                    , ansIsCorrect = HDBC.fromSql isCorrect
                    , ansAnsweredAt = HDBC.fromSql answeredAt
                    , ansCreatedAt = HDBC.fromSql createdAt
                    , ansModifiedAt = HDBC.fromSql modifiedAt
                    }
            _ -> error "Invalid row"
    sql =
        [r|
                select
                    id,
                    answer,
                    is_correct,
                    answered_at,
                    created_at,
                    modified_at
                from question_answer
                where
                    account = ?
                    and question = ?
                order by
                    answered_at desc
                limit 1000
            |]

-- TODO: We probably shouldn't error here.
getQuestionById :: (HasDbConn m, MonadIO m) => Text -> m Question
getQuestionById questionId =
    withConnection $ \conn -> do
        rows <- liftIO $ HDBC.quickQuery conn sql params
        case rows of
            [row] ->
                case row of
                    [id, question, answer, createdAt, modifiedAt] ->
                        pure $
                        Question
                            { qId = HDBC.fromSql id
                            , qQuestion = HDBC.fromSql question
                            , qAnswer = HDBC.fromSql answer
                            , qCreatedAt = HDBC.fromSql createdAt
                            , qModifiedAt = HDBC.fromSql modifiedAt
                            }
                    _ -> error "Invalid column count"
            _ -> error "Invalid number of rows"
  where
    params = [HDBC.toSql questionId]
    sql =
        [r|
                select
                    id,
                    question,
                    answer,
                    created_at,
                    modified_at
                from
                    question
                where
                    id = ?
            |]

getQuestionSet :: (HasDbConn m, MonadIO m) => Text -> Text -> m QuestionSet
getQuestionSet owner id =
    withConnection $ \conn -> do
        rows <- liftIO $ HDBC.quickQuery conn sql [HDBC.toSql owner, HDBC.toSql id]
        case rows of
            [row] -> pure $ rowToQuestionSet row
            _ -> error "Invalid number of rows returned from DB"
  where
    rowToQuestionSet row =
        case row of
            [sqlId, sqlName, sqlOwner, sqlCreatedAt, sqlModifiedAt] ->
                QuestionSet
                    { qsId = HDBC.fromSql sqlId
                    , qsName = HDBC.fromSql sqlName
                    , qsOwner = HDBC.fromSql sqlOwner
                    , qsCreatedAt = HDBC.fromSql sqlCreatedAt
                    , qsModifiedAt = HDBC.fromSql sqlModifiedAt
                    }
            _ -> error "Invalid number of columns"
    sql =
        [r|
            select id, name, owner, created_at, modified_at
            from question_set
            where owner = ?  and id = ?
        |]

getQuestionSetById :: (HasDbConn m, MonadIO m) => Text -> m QuestionSet
getQuestionSetById questionSetId =
    withConnection $ \conn -> do
        rows <- liftIO $ HDBC.quickQuery conn sql [HDBC.toSql questionSetId]
        case rows of
            [row] -> pure $ rowToQuestionSet row
            _ -> error "Invalid number of rows returned from DB"
  where
    rowToQuestionSet row =
        case row of
            [sqlId, sqlName, sqlOwner, sqlCreatedAt, sqlModifiedAt] ->
                QuestionSet
                    { qsId = HDBC.fromSql sqlId
                    , qsName = HDBC.fromSql sqlName
                    , qsOwner = HDBC.fromSql sqlOwner
                    , qsCreatedAt = HDBC.fromSql sqlCreatedAt
                    , qsModifiedAt = HDBC.fromSql sqlModifiedAt
                    }
            _ -> error "Invalid number of columns"
    sql =
        [r|
            select id, name, owner, created_at, modified_at
            from question_set
            where id = ?
        |]

getQuestionSetQuestions :: (HasDbConn m) => Text -> Text -> m [Question]
getQuestionSetQuestions owner id =
    withConnection $ \conn -> do
        rows <- liftIO $ HDBC.quickQuery conn sql params
        pure $ map rowToQuestion rows
  where
    params = [HDBC.toSql owner, HDBC.toSql owner, HDBC.toSql id]
    rowToQuestion row =
        case row of
            [id, question, answer, createdAt, modifiedAt, score] ->
                Question
                    { qId = HDBC.fromSql id
                    , qQuestion = HDBC.fromSql question
                    , qAnswer = HDBC.fromSql answer
                    , qScore = HDBC.fromSql score
                    , qCreatedAt = HDBC.fromSql createdAt
                    , qModifiedAt = HDBC.fromSql modifiedAt
                    }
            _ -> error "Can't convert a row to Question: invalid number of columns"
    sql =
        [r|
                select
                    id,
                    question,
                    answer,
                    created_at,
                    modified_at,
                    coalesce(
                        (
                            select
                                (
                                    avg(
                                        case is_correct
                                            when true then 1.0
                                            else 0.0
                                        end
                                    )
                                *
                                    (
                                        case
                                            when count(*) <= 5 then count(*) / 5.0
                                            else 1.0
                                        end
                                    )
                                ) as score
                            from
                                (
                                    select
                                        is_correct
                                    from
                                        question_answer
                                    where
                                        -- question.id from outer select
                                        question_answer.question = question.id
                                        and account = ?
                                    order by answered_at desc
                                    limit 100
                                ) as last_answer
                        ),
                        0
                    ) as score
                from
                    question
                where
                    question_set = (select id from question_set where owner = ? and id = ?)
                order by id
            |]

getRandomQuestion :: HasDbConn m => Text -> m Question
getRandomQuestion accId = do
    randomValue <- newId
    withConnection $ \conn -> do
        let params =
                [ HDBC.toSql randomValue
                , HDBC.toSql accId
                , HDBC.toSql randomValue
                , HDBC.toSql accId
                , HDBC.toSql accId
                ]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        case rows of
            [] -> error "No questions"
            _ -> do
                let questionsWithScores = map rowToQuestionWithScore rows
                let questionsWithWeights =
                        map (Data.Bifunctor.second scoreToWeight) questionsWithScores
                liftIO $
                    fprint
                        ("Choosing weighted from:\n" % text)
                        (formatQuestionWeights questionsWithWeights)
                randomQuestion <- liftIO $ weighted questionsWithWeights
                liftIO $ fprint ("Chosen: " % text % "\n") (qQuestion randomQuestion)
                pure randomQuestion
  where
    formatQuestionWeight (q, s) = format ((left 40 ' ' %. text) % " " % fixed 4) (qQuestion q) s
    formatQuestionWeights qw =
        Data.Text.Lazy.concat $ map ((\_l -> "  " <> _l <> "\n") . formatQuestionWeight) qw
    rowToQuestionWithScore =
        \case
            [id, question, answer, createdAt, modifiedAt, score] ->
                ( Question
                      { qId = HDBC.fromSql id
                      , qQuestion = HDBC.fromSql question
                      , qAnswer = HDBC.fromSql answer
                      , qCreatedAt = HDBC.fromSql createdAt
                      , qModifiedAt = HDBC.fromSql modifiedAt
                      }
                , HDBC.fromSql score :: Double)
            _ -> error "Invalid column count"
        -- Score 1 means well memorized -> 1 - (0.99 * 1) -> 1 - 0.99 -> 0.01
        -- Score 0 means poorly memorized -> 1 - (0.99 * 0) -> 1.0
        -- Score 0.5 means half memorized -> 1 - (0.99 * 0.5) -> 1 - 0.495 -> 0.505
    scoreToWeight score = toRational (1 - (0.99 * score))
        -- A query to select a number of questions with their scores, but a
        -- random subset of those.
    sql =
        [r|
                with
                    random_questions_per_user as (
                    select id
                    from
                        (
                            (
                                select id
                                from question
                                where
                                    id >= ?
                                    and question_set in (
                                        select id
                                        from question_set
                                        where
                                            owner = ?
                                            and (is_deleted = false or is_deleted is null)
                                    )
                                order by id
                            )
                            union all
                            (
                                select id
                                from question
                                where
                                    id < ?
                                    and question_set in (
                                        select id
                                        from question_set
                                        where
                                            owner = ?
                                            and (is_deleted = false or is_deleted is null)
                                    )
                                order by id
                            )
                        ) as r
                    limit 1000
                )
                select
                    r.id,
                    q.question,
                    q.answer,
                    q.created_at,
                    q.modified_at,
                    coalesce(
                        (
                            select
                                (
                                    avg(
                                        case is_correct
                                            when true then 1.0
                                            else 0.0
                                        end
                                    )
                                *
                                    (
                                        case
                                            when count(*) <= 5 then count(*) / 5.0
                                            else 1.0
                                        end
                                    )
                                ) as score
                            from
                                (
                                    select
                                        is_correct
                                    from
                                        question_answer
                                    where
                                        question = r.id
                                        and account = ?
                                    order by answered_at desc
                                    limit 100
                                ) as last_answer
                        ),
                        0
                    ) as score
                from
                    random_questions_per_user as r
                    join question as q on (q.id = r.id)
            |]

getSubscribedQuestionSetsForAccount :: HasDbConn m => Text -> m [QuestionSet]
getSubscribedQuestionSetsForAccount accId = do
    withConnection $ \conn -> do
        let params = [HDBC.toSql accId]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        pure $ map rowToQuestionSet rows
  where
    rowToQuestionSet =
        \case
            [id, name, owner, createdAt, modifiedAt] ->
                QuestionSet
                    { qsId = HDBC.fromSql id
                    , qsName = HDBC.fromSql name
                    , qsOwner = HDBC.fromSql owner
                    , qsCreatedAt = HDBC.fromSql createdAt
                    , qsModifiedAt = HDBC.fromSql modifiedAt
                    }
            _ -> error "Invaild number of columns"
    sql =
        [r|
                select
                    id, name, owner, created_at, modified_at
                from
                    question_set
                where
                    id in (select question_set from question_set_subscription where account = ?)
                    and (is_deleted = false or is_deleted is null)
                order by
                    id
            |]

deleteSessionValue :: HasDbConn m => Text -> Text -> m ()
deleteSessionValue sessionKey name =
    withConnection $ \conn -> do
        let params = [HDBC.toSql sessionKey, HDBC.toSql name]
        _ <- liftIO $ HDBC.run conn sql params
        liftIO $ HDBC.commit conn
  where
    sql =
        [r|
                delete from session_value
                where
                    session = (select id from session where key = ?)
                    and name = ?
            |]

ensureCsrfToken :: HasDbConn m => Text -> m Text
ensureCsrfToken sessionKey = do
    mExistingCsrfToken <-
        withConnection $ \conn -> do
            rows <- liftIO $ HDBC.quickQuery conn selectSql [HDBC.toSql sessionKey]
            case rows of
                [row] ->
                    case row of
                        [v] -> pure $ Just $ HDBC.fromSql v
                        _ -> error "Can't select CSRF token, invalid number of columns"
                _ -> pure Nothing
    case mExistingCsrfToken of
        Just token -> pure token
        Nothing -> do
            newCsrfToken <- newId
            newSessionValueId <- newId
            withConnection $ \conn -> do
                _ <-
                    liftIO $
                    HDBC.run
                        conn
                        insertSql
                        [ HDBC.toSql newSessionValueId
                        , HDBC.toSql sessionKey
                        , HDBC.toSql newCsrfToken
                        ]
                liftIO $ HDBC.commit conn
            withConnection $ \conn -> do
                rows <- liftIO $ HDBC.quickQuery conn selectSql [HDBC.toSql sessionKey]
                case rows of
                    [[v]] -> pure $ HDBC.fromSql v
                    _ -> error "Can't select CSRF token, even after upsert"
  where
    insertSql =
        [r|
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
                    'csrf_token',
                    ?,
                    current_timestamp,
                    current_timestamp
                )
                on conflict do nothing
            |]
    selectSql =
        [r|
                select value from session_value
                where
                    session = (select id from session where key = ?)
                    and name = 'csrf_token'
            |]

newId :: (MonadIO m) => m Text
newId =
    liftIO $
    Data.UUID.V4.nextRandom >>= \uuid -> pure $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid

searchQuestionSets :: (HasDbConn m, MonadIO m) => Text -> Text -> m [QuestionSet]
searchQuestionSets accId query = do
    withConnection $ \conn -> do
        let params = [HDBC.toSql accId, HDBC.toSql accId, HDBC.toSql query]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        pure $ map rowToSearchResult rows
  where
    rowToSearchResult =
        \case
            [id, name, owner, createdAt, modifiedAt] ->
                QuestionSet
                    { qsId = HDBC.fromSql id
                    , qsName = HDBC.fromSql name
                    , qsOwner = HDBC.fromSql owner
                    , qsCreatedAt = HDBC.fromSql createdAt
                    , qsModifiedAt = HDBC.fromSql modifiedAt
                    }
            _ -> error "Invalid column count"
    sql =
        [r|
                select
                    id,
                    name,
                    owner,
                    created_at,
                    modified_at
                from
                    question_set
                where
                    owner != ?
                    and id not in (
                        select question_set from question_set_subscription
                        where account = ?
                    )
                    and name like '%' || ? || '%'
                    and (
                        is_deleted = false  -- undeleted
                        or is_deleted is null  -- never deleted
                    )
                order by
                    created_at desc
            |]

sessionExists :: (Monad m, HasDb m) => Text -> m Bool
sessionExists sessionKey =
    withConnection $ \conn -> do
        let sql =
                [r|
                select created_at
                from session
                where key = ?
            |]
        let params = [HDBC.toSql sessionKey]
        rows <- liftIO $ HDBC.quickQuery conn sql params
        case rows of
            [row] ->
                case row of
                    [HDBC.SqlString _] -> pure True
                    [HDBC.SqlLocalTime _] -> pure True
                    v -> do
                        liftIO $ fprint ("sessionExists: Unexpected type: " % shown % "\n") v
                        pure True
            _ -> pure False

setQuestionSetDeleted :: (MonadIO m, HasDbConn m) => Text -> Text -> m ()
setQuestionSetDeleted owner questionSetId = do
    withConnection $ \conn -> do
        let params = [HDBC.toSql owner, HDBC.toSql questionSetId]
        liftIO $ HDBC.run conn sql params
        liftIO $ HDBC.commit conn
    pure ()
  where
    sql =
        [r|
                update question_set set is_deleted = true, deleted_at = current_timestamp
                where
                    owner = ?
                    and id = ?
            |]

subscribeQuestionSet :: (MonadIO m, HasDbConn m) => Text -> Text -> m ()
subscribeQuestionSet accId questionSetId = do
    withConnection $ \conn -> do
        qssId <- newId
        let params = [HDBC.toSql qssId, HDBC.toSql questionSetId, HDBC.toSql accId]
        liftIO $ HDBC.run conn sql params
        liftIO $ HDBC.commit conn
  where
    sql =
        [r|
                insert into question_set_subscription (
                    id,
                    question_set,
                    account,
                    subscribed,
                    created_at,
                    modified_at
                ) values (
                    ?,
                    ?,
                    ?,
                    true,
                    current_timestamp,
                    current_timestamp
                )
                on conflict (account, question_set)
                do
                    update
                    set
                        subscribed = true,
                        modified_at = current_timestamp
            |]

updateAnswer :: (MonadIO m, HasDbConn m) => Text -> Text -> (Bool, Text) -> m ()
updateAnswer accId answerId (isCorrect, answer) = do
    withConnection $ \conn -> do
        let params =
                [HDBC.toSql answer, HDBC.toSql isCorrect, HDBC.toSql answerId, HDBC.toSql accId]
        liftIO $ HDBC.run conn sql params
        liftIO $ HDBC.commit conn
  where
    sql =
        [r|
                update question_answer
                set
                    modified_at = current_timestamp,
                    answer = ?,
                    is_correct = ?
                where
                    id = ?
                    and account = ?
            |]
