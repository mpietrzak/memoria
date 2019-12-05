
module Memoria.Sessions
where

import Data.Text.Lazy (Text)

import qualified Memoria.Db as Memoria.Db

class Memoria.Db.HasDb m => HasSessions m where
    generateRandomSessionId :: m Text
    getSessionValue :: Text -> m (Maybe Text)
    setSessionValue :: Text -> Text -> m ()
