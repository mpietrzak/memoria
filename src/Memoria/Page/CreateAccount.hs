{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.CreateAccount (
    handleCreateAccount
) where

import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (liftIO)
import Formatting ((%), fprint, text)

import Memoria.Common (HasRedirects)
import Memoria.Cookies (HasCookies(setCookie))
import Memoria.Db (HasDb)
import Memoria.Sessions (HasSessions)
import qualified Memoria.Common
import qualified Memoria.Db
import qualified Memoria.Sessions

handleCreateAccount :: (HasCookies m, HasDb m, HasRedirects m, HasSessions m) => m Text
handleCreateAccount = do
    let sessionCookieName = "session_id"
    sessionKey <- Memoria.Sessions.createSession
    liftIO $ fprint
        ("handleCreateAccount: Created new session with key: " % text % "\n")
        sessionKey
    setCookie sessionCookieName sessionKey
    accountId <- Memoria.Db.createAccount
    Memoria.Sessions.setSessionValue "account_id" accountId
    Memoria.Common.redirect "/"
    pure "account created, redirecting to index"
