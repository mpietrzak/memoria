
module Memoria.Sessions
where


import Data.Text.Lazy (Text)


class HasSessions m where
    generateRandomSessionId :: m Text
    getSessionValue :: Text -> m (Maybe Text)
    setSessionValue :: Text -> Text -> m ()

