{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Memoria (main) where

import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Default.Class (def)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Data.UUID as Data.UUID
import qualified Data.UUID.V4 as Data.UUID.V4;
import qualified Web.Scotty.Trans as ST
import Data.Pool (Pool, createPool, takeResource, withResource)
import qualified Database.HDBC.PostgreSQL as PSQL

import Memoria.Page.Index (handleIndex)
import Memoria.Sessions (HasSessions, generateRandomSessionId, getSessionValue, setSessionValue)
import qualified Memoria.Conf as Memoria.Conf
import qualified Memoria.Db as Memoria.Db

data State = State { stateDbPool :: Pool PSQL.Connection }

newtype StateM a = StateM
  { runStateM :: ReaderT State IO a
  } deriving (Applicative, Functor, Monad, MonadBase IO, MonadBaseControl IO, MonadIO, MonadReader State)

application :: ST.ScottyT Text StateM ()
application = do
    ST.get "/" $ do
        body <- handleIndex
        ST.html body

instance HasSessions (ST.ActionT Text StateM) where
    generateRandomSessionId = do
        uuid <- liftIO $ Data.UUID.V4.nextRandom
        return $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid
    getSessionValue name = do
        error "not implemented yet"
    setSessionValue name value = do
        error "not implemented yet"

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
    ST.scottyOptsT def runIO application

