
module Memoria.Sessions (
    HasSessions (
        createSession,
        generateRandomSessionKey,
        getSessionValue,
        ensureSession,
        setSessionValue
    )
)
where

import Data.Text.Lazy (Text)

import qualified Memoria.Db as Memoria.Db

class Memoria.Db.HasDb m => HasSessions m where
    createSession :: m Text
    generateRandomSessionKey :: m Text
    getSessionValue :: Text -> m (Maybe Text)
    ensureSession :: m Text
    setSessionValue :: Text -> Text -> m ()
