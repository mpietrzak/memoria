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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Memoria
    ( loadConf
    , main
    ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Lazy (MonadState, StateT, get, put, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Lazy
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Map.Lazy
import Data.Pool (Pool, withResource)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Database.HDBC.PostgreSQL as PSQL
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp
import qualified Web.Cookie
import qualified Web.Scotty.Cookie
import qualified Web.Scotty.Trans as ST

import Memoria.Common
    ( HasParams(getParam)
    , HasRedirects
    , HasRequestMethod
    , SysStats(..)
    , getRequestMethod
    )
import qualified Memoria.Common as Memoria.Common
import qualified Memoria.Conf as Memoria.Conf
import Memoria.Cookies (HasCookies(getCookie, setCookie))
import qualified Memoria.Db as Memoria.Db
import Memoria.Page.Auth (handleAuth)
import Memoria.Page.CreateAccount (handleCreateAccount)
import Memoria.Page.CreateQuestion (handleCreateQuestion)
import Memoria.Page.CreateQuestionSet (handleCreateQuestionSet)
import Memoria.Page.DeleteQuestionSet (handleDeleteQuestionSet)
import Memoria.Page.EditAnswer (handleEditAnswer)
import Memoria.Page.EditQuestionSet (handleEditQuestionSet)
import Memoria.Page.Export (handleExport, handleExportQuestions)
import Memoria.Page.Index (handleIndex)
import Memoria.Page.Login (handleLogin)
import Memoria.Page.Logout (handleLogout)
import Memoria.Page.QuestionAnswers (handleQuestionAnswers)
import Memoria.Page.QuestionSet (handleQuestionSet)
import Memoria.Page.QuestionSetSubscribe (handleQuestionSetSubscribe)
import Memoria.Page.Search (handleSearch)
import Memoria.Page.Settings (handleDeleteNickname)
import Memoria.Page.Settings (handleSetNickname)
import Memoria.Page.Settings (handleSettings)
import Memoria.Page.Settings (handleSettingsAddEmail)
import Memoria.Page.SubscribeQuestionSet (handleSubscribeQuestionSet)
import Memoria.Page.Test (handleTest)
import Memoria.Page.Test (handleTestAnswer)
import Memoria.Sessions
    ( HasSessions
    , createSession
    , deleteSessionValue
    , ensureSession
    , generateRandomSessionKey
    , getSessionValue
    , sessionIdCookieName
    , setSessionValue
    )
import qualified Memoria.Timers

data State =
    State
        { stateCookies :: Maybe (Data.Map.Lazy.Map Text Text)
        , stateDbPool :: Pool PSQL.Connection
        , stateSysStatsRef :: IORef SysStats
        }

newtype StateM a =
    StateM
        { runStateM :: StateT State IO a
        }
    deriving ( Applicative
             , Functor
             , Monad
             , MonadBase IO
             , MonadBaseControl IO
             , MonadIO
             , MonadState State
             )

instance HasCookies (ST.ActionT Text StateM) where
    getCookie cookieName = do
        state <- lift get
        case stateCookies state of
            Just cookies ->
                case Data.Map.Lazy.lookup cookieName cookies of
                    Just stateCookie
                    -- Cookie already exists in this request's state cookies, just return it.
                     -> do
                        pure $ Just stateCookie
                    Nothing
                    -- Not found in map, try to find in request
                     -> do
                        mReqCookie <-
                            Web.Scotty.Cookie.getCookie (Data.Text.Lazy.toStrict cookieName)
                        case mReqCookie of
                            Just reqCookieStrict -> do
                                let reqCookie = Data.Text.Lazy.fromStrict reqCookieStrict
                                let cookies' = Data.Map.Lazy.insert cookieName reqCookie cookies
                                let state' = state {stateCookies = Just cookies'}
                                lift $ put state'
                                pure $ Just reqCookie
                            Nothing -> pure Nothing
            Nothing
                -- Cookies not found (yet) in state.
             -> do
                mReqCookie <- Web.Scotty.Cookie.getCookie (Data.Text.Lazy.toStrict cookieName)
                case mReqCookie of
                    Just reqCookieStrict -> do
                        let reqCookie = Data.Text.Lazy.fromStrict reqCookieStrict
                        let cookies = Data.Map.Lazy.empty
                        let cookies' = Data.Map.Lazy.insert cookieName reqCookie cookies
                        let state' = state {stateCookies = Just cookies'}
                        lift $ put state'
                        pure $ Just reqCookie
                    Nothing -> pure Nothing
    setCookie cookieName cookieValue = do
        state <- lift get
        let cookies =
                case stateCookies state of
                    Nothing -> Data.Map.Lazy.empty
                    Just c -> c
        let cookies' = Data.Map.Lazy.insert cookieName cookieValue cookies
        let state' = state {stateCookies = Just cookies'}
        lift $ put state'

instance Memoria.Common.HasCsrfToken (ST.ActionT Text StateM) where
    checkCsrfToken token = do
        goodCsrfToken <- getSessionValue "csrf_token"
        case Just token == goodCsrfToken of
            True -> pure ()
            False -> error "CSRF does not match"
    ensureCsrfToken = do
        sessionKey <- ensureSession
        Memoria.Db.ensureCsrfToken sessionKey

instance Memoria.Common.HasFooterStats (ST.ActionT Text StateM)

instance HasParams (ST.ActionT Text StateM) where
    getParam name = do
        params <- ST.params
        let val = lookup name params
        pure val

instance HasRedirects (ST.ActionT Text StateM) where
    redirect loc = do
        state <- lift get
        case stateCookies state of
            Nothing -> return ()
            Just cookies -> setResponseCookies $ Data.Map.Lazy.toList cookies
        ST.redirect loc

instance HasRequestMethod (ST.ActionT Text StateM) where
    getRequestMethod = do
        request <- ST.request
        pure $ Network.Wai.requestMethod request

instance HasSessions (ST.ActionT Text StateM) where
    createSession = do
        sessionKey <- generateRandomSessionKey
        Memoria.Db.createSession sessionKey
        pure sessionKey
    deleteSessionValue name = do
        sessionKey <- ensureSession
        Memoria.Db.deleteSessionValue sessionKey name
    generateRandomSessionKey = do
        uuid <- liftIO $ Data.UUID.V4.nextRandom
        return $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid
    getSessionValue name = do
        mSessionKey <- getCookie sessionIdCookieName
        case mSessionKey of
            Nothing -> pure Nothing
            Just sessionKey -> Memoria.Db.getSessionValue sessionKey name
    setSessionValue name value = do
        sessionKey <- ensureSession
        _ <- Memoria.Db.setSessionValue sessionKey name value
        pure ()
    ensureSession
        -- Check if cookie session is ok, create a new one if needed.
     = do
        mCookieSessionKey <- getCookie sessionIdCookieName
        case mCookieSessionKey of
            Nothing
              -- No session cookie at all, just generate new
             -> do
                sessionKey <- generateRandomSessionKey
                Memoria.Db.createSession sessionKey
                setCookie sessionIdCookieName sessionKey
                pure sessionKey
            Just cookieSessionKey
              -- Session cookie found, but might be broken
             -> do
                sessionExists <- Memoria.Db.sessionExists cookieSessionKey
                case sessionExists of
                    True
                    -- Cookie exists, session exists, but we should probably
                    -- poke it a little bit (todo).
                     -> do
                        pure cookieSessionKey
                    False -> do
                        key <- createSession
                        setCookie sessionIdCookieName key
                        pure key

instance Memoria.Common.HasSysStats (ST.ActionT Text StateM) where
    getSysStats = do
        state <- lift $ get
        let statsRef = stateSysStatsRef state
        sysStats <- liftIO $ readIORef statsRef
        pure sysStats

instance Memoria.Db.HasDbConn (ST.ActionT Text StateM) where
    withConnection action = do
        state <- lift $ get
        let pool = stateDbPool state
        withResource pool action

instance Memoria.Db.HasDb (ST.ActionT Text StateM)

instance Memoria.Common.HasAccounts (ST.ActionT Text StateM)

application :: ST.ScottyT Text StateM ()
application = do
    ST.get "/" $ withSetCookies handleIndex >>= ST.html
    ST.get "/auth" $ withSetCookies handleAuth >>= ST.html
    ST.get "/create-account" $ withSetCookies handleCreateAccount >>= ST.html
    ST.get "/create-question" $ withSetCookies handleCreateQuestion >>= ST.html
    ST.get "/create-question-set" $ withSetCookies handleCreateQuestionSet >>= ST.html
    ST.get "/delete-nickname" $ withSetCookies handleDeleteNickname >>= ST.html
    ST.get "/delete-question-set" $ withSetCookies handleDeleteQuestionSet >>= ST.html
    ST.get "/edit-answer" $ withSetCookies handleEditAnswer >>= ST.html
    ST.get "/edit-question-set" $ withSetCookies handleEditQuestionSet >>= ST.html
    ST.get "/export" $ withSetCookies handleExport >>= ST.html
    ST.get "/login" $ withSetCookies handleLogin >>= ST.html
    -- TODO: GET on logout should not logout
    ST.get "/logout" $ withSetCookies handleLogout >>= ST.html
    ST.get "/question-answers" $ withSetCookies handleQuestionAnswers >>= ST.html
    ST.get "/question-set" $ withSetCookies handleQuestionSet >>= ST.html
    ST.get "/question-set-search" $ withSetCookies handleSearch >>= ST.html
    ST.get "/question-set-subscribe" $ withSetCookies handleQuestionSetSubscribe >>= ST.html
    ST.get "/set-nickname" $ withSetCookies handleSetNickname >>= ST.html
    ST.get "/settings" $ withSetCookies handleSettings >>= ST.html
    ST.get "/settings-add-email" $ withSetCookies handleSettingsAddEmail >>= ST.html
    ST.get "/subscribe-question-set" $ withSetCookies handleSubscribeQuestionSet >>= ST.html
    ST.get "/test" $ withSetCookies handleTest >>= ST.html
    ST.post "/answer" $ withSetCookies handleTestAnswer >>= ST.html
    ST.post "/create-question" $ withSetCookies handleCreateQuestion >>= ST.html
    ST.post "/create-question-set" $ withSetCookies handleCreateQuestionSet >>= ST.html
    ST.post "/delete-nickname" $ withSetCookies handleDeleteNickname >>= ST.html
    ST.post "/delete-question-set" $ withSetCookies handleDeleteQuestionSet >>= ST.html
    ST.post "/edit-answer" $ withSetCookies handleEditAnswer >>= ST.html
    ST.post "/edit-question-set" $ withSetCookies handleEditQuestionSet >>= ST.html
    ST.post "/login" $ withSetCookies handleLogin >>= ST.html
    ST.post "/question-set-subscribe" $ withSetCookies handleQuestionSetSubscribe >>= ST.html
    ST.post "/settings-add-email" $ withSetCookies handleSettingsAddEmail >>= ST.html
    ST.post "/set-nickname" $ withSetCookies handleSetNickname >>= ST.html
    ST.post "/subscribe-question-set" $ withSetCookies handleSubscribeQuestionSet >>= ST.html
    ST.get "/export-questions" $ do
        exportJson <- withSetCookies handleExportQuestions
        ST.setHeader "content-type" "text/json"
        ST.setHeader "content-disposition" "attachment; filename=\"memoria.json\""
        ST.raw exportJson
  where
    withSetCookies a = do
        r <- a
        state <- lift get
        case stateCookies state of
            Nothing -> pure r
            Just cookies -> do
                setResponseCookies $ Data.Map.Lazy.toList cookies
                pure r

loadConf :: Text -> IO (Either Text Memoria.Conf.Conf)
loadConf = Memoria.Conf.load

main :: Memoria.Conf.Conf -> IO ()
main conf = do
    sysStatsRef <- newIORef def
    pool <-
        Memoria.Db.createDbPool
            (Memoria.Conf.cfgDbHost conf)
            (Memoria.Conf.cfgDbPort conf)
            (Memoria.Conf.cfgDbName conf)
            (Memoria.Conf.cfgDbUser conf)
            (Memoria.Conf.cfgDbPass conf)
    Memoria.Timers.startTimers pool sysStatsRef
    let state = State {stateCookies = Nothing, stateDbPool = pool, stateSysStatsRef = sysStatsRef}
    let runIO m = do
            (result, _) <- runStateT (runStateM m) state
            pure result
    let warpSettings =
            Network.Wai.Handler.Warp.setPort (Memoria.Conf.cfgPort conf) $
            Network.Wai.Handler.Warp.defaultSettings
    let options = def {ST.settings = warpSettings}
    ST.scottyOptsT options runIO application

setResponseCookies :: (Monad m, MonadIO m, ST.ScottyError e) => [(Text, Text)] -> ST.ActionT e m ()
setResponseCookies cookies =
    for_ cookies $ \(k, v) -> do
        let cookieMaxAge = fromInteger (3600 * 24 * 30 * 12)
        let toStrictBS = Data.ByteString.Lazy.toStrict . Data.Text.Lazy.Encoding.encodeUtf8
        let cookie =
                def
                    { Web.Cookie.setCookieName = toStrictBS k
                    , Web.Cookie.setCookieValue = toStrictBS v
                    , Web.Cookie.setCookieMaxAge = (Just cookieMaxAge)
                    }
        Web.Scotty.Cookie.setCookie cookie
