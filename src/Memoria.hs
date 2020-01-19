{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Memoria (loadConf, main) where

import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Lazy (MonadState, StateT, runStateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Pool (Pool, withResource)
import Data.Text.Lazy (Text)
import Formatting ((%), fprint, shown, text)
import qualified Data.ByteString.Lazy
import qualified Data.Map.Lazy
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

import Memoria.Common (HasParams(getParam), HasRedirects, HasRequestMethod, getRequestMethod)
import Memoria.Cookies (HasCookies(getCookie, setCookie))
import Memoria.Page.Auth (handleAuth)
import Memoria.Page.CreateAccount (handleCreateAccount)
import Memoria.Page.CreateQuestion (handleCreateQuestion)
import Memoria.Page.CreateQuestionSet (handleCreateQuestionSet)
import Memoria.Page.Index (handleIndex)
import Memoria.Page.Login (handleLogin)
import Memoria.Page.QuestionSet (handleQuestionSet)
import Memoria.Page.Settings (handleSettings)
import Memoria.Page.Settings (handleSettingsAddEmail)
import Memoria.Page.Test (handleTest)
import Memoria.Sessions ( HasSessions, createSession, generateRandomSessionKey, getSessionValue, ensureSession, setSessionValue)
import qualified Memoria.Common as Memoria.Common
import qualified Memoria.Conf as Memoria.Conf
import qualified Memoria.Db as Memoria.Db

sessionCookieName = "session_id" :: Text

data State = State { stateCookies :: Maybe (Data.Map.Lazy.Map Text Text)
                   , stateDbPool :: Pool PSQL.Connection }

newtype StateM a = StateM
  { runStateM :: StateT State IO a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadBase IO
             , MonadBaseControl IO
             , MonadIO
             , MonadState State )

instance HasCookies (ST.ActionT Text StateM) where
    getCookie cookieName = do
        state <- lift get
        case stateCookies state of
            Just cookies -> case Data.Map.Lazy.lookup cookieName cookies of
                Just stateCookie -> do
                    -- Cookie already exists in this request's state cookies, just return it.
                    pure $ Just stateCookie
                Nothing -> do
                    -- Not found in map, try to find in request
                    mReqCookie <- Web.Scotty.Cookie.getCookie (Data.Text.Lazy.toStrict cookieName)
                    case mReqCookie of
                        Just reqCookieStrict -> do
                            let reqCookie = Data.Text.Lazy.fromStrict reqCookieStrict
                            let cookies' = Data.Map.Lazy.insert cookieName reqCookie cookies
                            let state' = state { stateCookies = Just cookies' }
                            lift $ put state'
                            pure $ Just reqCookie
                        Nothing -> pure Nothing
            Nothing -> do
                -- Cookies not found (yet) in state.
                liftIO $ fprint
                    ("getCookie: Cookies not yet in state\n")
                mReqCookie <- Web.Scotty.Cookie.getCookie (Data.Text.Lazy.toStrict cookieName)
                case mReqCookie of
                    Just reqCookieStrict -> do
                        let reqCookie = Data.Text.Lazy.fromStrict reqCookieStrict
                        let cookies = Data.Map.Lazy.empty
                        let cookies' = Data.Map.Lazy.insert cookieName reqCookie cookies
                        let state' = state { stateCookies = Just cookies' }
                        lift $ put state'
                        pure $ Just reqCookie
                    Nothing -> pure Nothing
    setCookie cookieName cookieValue = do
        liftIO $ fprint
            ("setCookie: About to put cookie (" % text % " -> " % text % ") into state\n")
            cookieName
            cookieValue
        state <- lift get
        let cookies = case stateCookies state of
                Nothing -> Data.Map.Lazy.empty
                Just c -> c
        let cookies' = Data.Map.Lazy.insert cookieName cookieValue cookies
        liftIO $ fprint
            ("setCookie: Saving cookies in state: " % shown % "\n")
            cookies'
        let state' = state { stateCookies = Just cookies' }
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
    generateRandomSessionKey = do
        uuid <- liftIO $ Data.UUID.V4.nextRandom
        return $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid
    getSessionValue name = do
        let sessionCookieName = "session_id"
        mSessionKey <- getCookie sessionCookieName
        liftIO $ fprint ("getSessionValue: mSessionKey: " % shown % "\n") mSessionKey
        case mSessionKey of
          Nothing -> pure Nothing
          Just sessionKey -> Memoria.Db.getSessionValue sessionKey name
    setSessionValue name value = do
        sessionKey <- ensureSession
        liftIO $ fprint
            ("setSessionValue: Ensured session with key: " % text % "\n")
            sessionKey
        _ <- Memoria.Db.setSessionValue sessionKey name value
        pure ()
    ensureSession = do
        -- Check if cookie session is ok, create a new one if needed.
        mCookieSessionKey <- getCookie sessionCookieName
        case mCookieSessionKey of
          Nothing -> do
              -- No session cookie at all, just generate new
              sessionKey <- generateRandomSessionKey
              Memoria.Db.createSession sessionKey
              setCookie sessionCookieName sessionKey
              pure sessionKey
          Just cookieSessionKey -> do
              -- Session cookie found, but might be broken
              sessionExists <- Memoria.Db.sessionExists cookieSessionKey
              case sessionExists of
                True -> do
                    -- Cookie exists, session exists, but we should probably
                    -- poke it a little bit (todo).
                    liftIO $ fprint
                        ("setSessionValue: Session for " % text % " exists\n")
                        cookieSessionKey
                    pure cookieSessionKey
                False -> do
                    key <- createSession
                    setCookie sessionCookieName key
                    pure key


instance Memoria.Db.HasDbConn (ST.ActionT Text StateM) where
    getConnection = do
        error "not implemented yet"
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
    ST.get "/login" $ withSetCookies handleLogin >>= ST.html
    ST.get "/question-set" $ withSetCookies handleQuestionSet >>= ST.html
    ST.get "/settings" $ withSetCookies handleSettings >>= ST.html
    ST.get "/settings-add-email" $ withSetCookies handleSettingsAddEmail >>= ST.html
    ST.get "/test" $ withSetCookies handleTest >>= ST.html
    ST.post "/create-question" $ withSetCookies handleCreateQuestion >>= ST.html
    ST.post "/create-question-set" $ withSetCookies handleCreateQuestionSet >>= ST.html
    ST.post "/login" $ withSetCookies handleLogin >>= ST.html
    ST.post "/settings-add-email" $ withSetCookies handleSettingsAddEmail >>= ST.html
    where
        withSetCookies a = do
            liftIO $ fprint ("application.withSetCookies: Running action\n")
            r <- a
            state <- lift get
            liftIO $ fprint
                ("application.withSetCookies: State cookies after action: " % shown % "\n")
                (stateCookies state)
            case stateCookies state of
                Nothing -> pure r
                Just cookies -> do
                    setResponseCookies $ Data.Map.Lazy.toList cookies
                    pure r

loadConf :: Text -> IO (Either Text Memoria.Conf.Conf)
loadConf = Memoria.Conf.load

main :: Memoria.Conf.Conf -> IO ()
main conf = do
    pool <- Memoria.Db.createDbPool
        (Memoria.Conf.cfgDbHost conf)
        (Memoria.Conf.cfgDbPort conf)
        (Memoria.Conf.cfgDbName conf)
        (Memoria.Conf.cfgDbUser conf)
        (Memoria.Conf.cfgDbPass conf)
    let state = State { stateCookies = Nothing, stateDbPool = pool }
    let runIO m = do
            (result, _) <- runStateT (runStateM m) state
            pure result
    let warpSettings = Network.Wai.Handler.Warp.setPort (Memoria.Conf.cfgPort conf)
            $ Network.Wai.Handler.Warp.defaultSettings
    let options = def { ST.settings = warpSettings }
    ST.scottyOptsT options runIO application


setResponseCookies :: (Monad m, MonadIO m, ST.ScottyError e) => [(Text, Text)] -> ST.ActionT e m ()
setResponseCookies cookies = for_ cookies $ \(k,v) -> do
        liftIO $ fprint
            ("setResponseCookies: Setting response cookie '" % text % "' -> '" % text % "'\n")
            k
            v
        let cookieMaxAge = fromInteger (3600 * 24 * 30 * 12)
        let toStrictBS = Data.ByteString.Lazy.toStrict
                . Data.Text.Lazy.Encoding.encodeUtf8
        let cookie = def { Web.Cookie.setCookieName = toStrictBS k
                         , Web.Cookie.setCookieValue = toStrictBS v
                         , Web.Cookie.setCookieMaxAge = (Just cookieMaxAge) }
        Web.Scotty.Cookie.setCookie cookie

