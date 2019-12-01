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

import qualified Memoria.Conf as M.Conf
import qualified Web.Scotty.Trans as ST
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Data.Default.Class (def)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Data.UUID as Data.UUID
import qualified Data.UUID.V4 as Data.UUID.V4;
import Control.Monad.IO.Class (liftIO)

import Memoria.Page.Index (handleIndex)
import Memoria.Sessions (HasSessions, generateRandomSessionId, getSessionValue, setSessionValue)

data State = State

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

main :: M.Conf.Conf -> IO ()
main conf = do
    {-
    pool <- createDbPool
        (cfgDbHost conf)
        (cfgDbPort conf)
        (cfgDbName conf)
        (cfgDbUser conf)
        (cfgDbPass conf)
    -}
    let state = State
    let runIO m = runReaderT (runStateM m) state
    ST.scottyOptsT def runIO application
