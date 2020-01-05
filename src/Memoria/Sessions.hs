
module Memoria.Sessions (
    HasSessions(
    createSession,
    generateRandomSessionKey,
    getSessionValue,
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
    setSessionValue :: Text -> Text -> m ()
