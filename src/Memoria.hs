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
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Default.Class (def)
import Data.Pool (Pool, createPool, takeResource, withResource)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Data.UUID as Data.UUID
import qualified Data.UUID.V4 as Data.UUID.V4;
import qualified Database.HDBC.PostgreSQL as PSQL
import qualified Network.Wai.Handler.Warp
import qualified Web.Scotty.Trans as ST
import qualified Web.Scotty.Cookie as Web.Scotty.Cookie

import Memoria.Page.Index (handleIndex)
import Memoria.Sessions (HasSessions, generateRandomSessionId, getSessionValue, setSessionValue)
import qualified Memoria.Common as Memoria.Common
import qualified Memoria.Conf as Memoria.Conf
import qualified Memoria.Db as Memoria.Db

data State = State { stateDbPool :: Pool PSQL.Connection }

newtype StateM a = StateM
  { runStateM :: ReaderT State IO a
  } deriving (Applicative, Functor, Monad, MonadBase IO, MonadBaseControl IO, MonadIO, MonadReader State)

class HasCookies m where
    getCookie :: Text -> m (Maybe Text)
    setCookie :: Text -> Text -> m ()

instance HasCookies (ST.ActionT Text StateM) where
    getCookie cookieName = do
        mc <- Web.Scotty.Cookie.getCookie (Data.Text.Lazy.toStrict cookieName)
        case mc of
          Just v -> pure $ Just $ Data.Text.Lazy.fromStrict v
          Nothing -> pure Nothing
    setCookie cokoieName cookieValue = error "oops"

instance HasSessions (ST.ActionT Text StateM) where
    generateRandomSessionId = do
        uuid <- liftIO $ Data.UUID.V4.nextRandom
        return $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid
    getSessionValue name = do
        let sessionCookieName = "session_id"
        mSessionId <- getCookie sessionCookieName
        case mSessionId of
          Nothing -> pure Nothing
          Just sessionId -> Memoria.Db.getSessionValue sessionId name
    setSessionValue name value = do
        error "not implemented yet"

instance Memoria.Db.HasDbConn (ST.ActionT Text StateM) where
    getConnection = do
        error "not implemented yet"
    withConnection action = do
        pool <- lift $ asks stateDbPool
        withResource pool action

instance Memoria.Db.HasDb (ST.ActionT Text StateM)

instance Memoria.Common.HasAccounts (ST.ActionT Text StateM)

application :: ST.ScottyT Text StateM ()
application = do
    ST.get "/" $ do
        body <- handleIndex
        ST.html body

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
    let state = State { stateDbPool = pool }
    let runIO m = runReaderT (runStateM m) state
    let warpSettings = Network.Wai.Handler.Warp.setPort (Memoria.Conf.cfgPort conf)
            $ Network.Wai.Handler.Warp.defaultSettings
    let options = def { ST.settings = warpSettings }
    ST.scottyOptsT options runIO application

