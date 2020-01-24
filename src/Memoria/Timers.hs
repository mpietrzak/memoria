{-# LANGUAGE OverloadedStrings #-}

module Memoria.Timers (
    startTimer,
    timerMain
) where

import Data.Pool (Pool)
import Formatting (fprint)
import qualified Control.Concurrent.Suspend
import qualified Control.Concurrent.Timer
import qualified Database.HDBC.PostgreSQL as PSQL

startTimer :: Pool PSQL.Connection -> IO ()
startTimer pool = do
    let delay = Control.Concurrent.Suspend.hDelay 24
    _timer <- Control.Concurrent.Timer.repeatedTimer
        (timerMain pool)
        delay
    pure ()

timerMain :: Pool PSQL.Connection -> IO ()
timerMain _pool = fprint "Running timer...\n"

