{-# LANGUAGE OverloadedStrings #-}

module Memoria.Common (
    HasAccounts,
    HasCsrfToken(..),
    HasFooterStats(getFooterStats),
    HasParams(getParam),
    HasRedirects(redirect),
    HasRequestMethod,
    HasSetSysStats(setSysStatsResidentSetSize, setSysStatsDatabaseSize),
    HasSysStats(..),
    SysStats(..),
    getAccountId,
    getRequestMethod,
    hasAccount
) where

import Data.Default.Class (Default, def)
import Data.Text.Lazy (Text)
import Formatting ((%), format, fixed, text)
import qualified Network.HTTP.Types.Method

import Memoria.Sessions (HasSessions, getSessionValue, sessionAccountIdName)
import Memoria.View.Base (FooterStats(..))

data SysStats = SysStats { sDatabaseSize :: Maybe Integer
                         , sResidentSetSize :: Maybe Integer }

instance Default SysStats where def = SysStats { sDatabaseSize = Nothing
                                               , sResidentSetSize = Nothing }

class HasCsrfToken m where
    checkCsrfToken :: Text -> m ()
    ensureCsrfToken :: m Text

class HasSysStats m => HasFooterStats m where
    getFooterStats :: m FooterStats
    getFooterStats = do
        sysStats <- getSysStats
        pure $ FooterStats { fDatabaseSize = sDatabaseSize sysStats
                           , fResidentSetSize = sResidentSetSize sysStats }

class HasParams m where
    getParam :: Text -> m (Maybe Text)

class HasRedirects m where
    redirect :: Text -> m ()

class HasRequestMethod m where
    getRequestMethod :: m Network.HTTP.Types.Method.Method

class HasSessions m => HasAccounts m
    where
        getAccountId :: m (Maybe Text)
        getAccountId = do
            mAccountId <- getSessionValue sessionAccountIdName
            pure $ mAccountId
        hasAccount :: m Bool
        hasAccount = do
            mAccountId <- getSessionValue sessionAccountIdName
            case mAccountId of
              Nothing -> pure False
              Just _ -> pure True

class HasSetSysStats m where
    setSysStatsDatabaseSize :: Integer -> m ()
    setSysStatsResidentSetSize :: Integer -> m ()

class Monad m => HasSysStats m where
    getSysStats :: m SysStats

