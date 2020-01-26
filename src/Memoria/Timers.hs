{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Memoria.Timers (
    startTimers
) where

import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, Reader, ask, lift, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Pool (Pool)
import Data.Pool (withResource)
import Formatting ((%), fprint, shown)
import qualified Control.Concurrent.Suspend
import qualified Control.Concurrent.Timer
import qualified Database.HDBC.PostgreSQL as PSQL

import Memoria.Common (HasSetSysStats(setSysStatsResidentSetSize, setSysStatsDatabaseSize), SysStats(..))
import Memoria.Db (HasDb, HasDbConn, withConnection)
import qualified Memoria.Common
import qualified Memoria.Db
import qualified Memoria.Sys

data TimerEnv = TimerEnv { tDbPool :: Pool PSQL.Connection, tStatsRef :: IORef SysStats }

instance HasDb (ReaderT TimerEnv IO)

instance HasDbConn (ReaderT TimerEnv IO) where
    withConnection act = do
        env <- ask
        let dbPool = tDbPool env
        withResource dbPool act

instance HasSetSysStats (ReaderT TimerEnv IO) where
    setSysStatsResidentSetSize size = do
        env <- ask
        let statsRef = tStatsRef env
        liftIO $ atomicModifyIORef'
            statsRef
            (\stats -> (stats { sResidentSetSize = Just size }, ()))
    setSysStatsDatabaseSize size = do
        env <- ask
        let statsRef = tStatsRef env
        liftIO $ atomicModifyIORef'
            statsRef
            (\stats -> (stats { sDatabaseSize = Just size }, ()))

startTimers :: Pool PSQL.Connection -> IORef SysStats -> IO ()
startTimers pool statsRef = do
    startSysStatsTimer pool statsRef

startSysStatsTimer pool statsRef = do
    statsTimerMain pool statsRef
    let delay = Control.Concurrent.Suspend.sDelay 60
    _timer <- Control.Concurrent.Timer.repeatedTimer
        (statsTimerMain pool statsRef)
        delay
    pure ()

statsTimerMain :: Pool PSQL.Connection -> IORef SysStats -> IO ()
statsTimerMain pool statsRef = do
    let env = TimerEnv { tDbPool = pool, tStatsRef = statsRef }
    result <- runReaderT statsTimerMainM env
    pure ()

statsTimerMainM :: (MonadIO m, Memoria.Db.HasDb m, Memoria.Common.HasSetSysStats m) => m ()
statsTimerMainM = do
    Memoria.Db.getDbSize >>= \case
        Left err -> liftIO $ fprint ("statsTimerMainM: Error getting DB size: " % shown % "\n") err
        Right dbSize -> setSysStatsDatabaseSize dbSize
    (liftIO $ Memoria.Sys.getCurrentResidentSetSize) >>= \case
        0 -> pure ()
        rss -> setSysStatsResidentSetSize rss
    pure ()

