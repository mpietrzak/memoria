{-# LANGUAGE OverloadedStrings #-}

module Memoria.Sessions (
    HasSessions (
        createSession,
        deleteSessionValue,
        ensureSession,
        generateRandomSessionKey,
        getSessionValue,
        setSessionValue
    ),
    sessionAccountIdName,
    sessionIdCookieName
)
where

import Data.Text.Lazy (Text)

import qualified Memoria.Db

sessionAccountIdName :: Text
sessionAccountIdName = "account_id"

sessionIdCookieName :: Text
sessionIdCookieName = "session_id"

class Memoria.Db.HasDb m => HasSessions m where
    createSession :: m Text
    ensureSession :: m Text
    generateRandomSessionKey :: m Text
    getSessionValue :: Text -> m (Maybe Text)
    deleteSessionValue :: Text -> m ()
    setSessionValue :: Text -> Text -> m ()
